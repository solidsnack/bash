{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , RecordWildCards
           , NamedFieldPuns
           , NoMonomorphismRestriction
           , GeneralizedNewtypeDeriving
           , UndecidableInstances
  #-}
{-| Pretty printer for Bash.
 -}
module Language.Bash.PrettyPrinter where

import Control.Applicative
import qualified Data.List as List
import Data.ByteString.Char8
import Data.Binary.Builder (Builder)
import Data.Monoid
import Prelude hiding ( words, unwords, concat, null
                      , replicate, lines, drop, length )
import Control.Monad.State.Strict

import qualified Text.ShellEscape as Esc

import Language.Bash.Syntax
import Language.Bash.PrettyPrinter.State


bytes                       ::  (PP t) => t -> ByteString
bytes                        =  renderBytes (nlCol 0) . pp

builder                     ::  (PP t) => t -> Builder
builder                      =  render (nlCol 0) . pp

bytes_state                  =  renderBytes (nlCol 0)


class Annotation t where
  annotate                  ::  t -> Statement t -> State PPState ()
instance Annotation () where
  annotate _ stmt            =  pp stmt

class PP t where
  pp                        ::  t -> State PPState ()
instance PP Identifier where
  pp (Identifier b)          =  word b
instance PP FuncName where
  pp (Simple ident)          =  pp ident
  pp (Fancy b)               =  word b
instance PP SpecialVar where
  pp                         =  word . specialVarBytes
instance PP FileDescriptor where
  pp (FileDescriptor w)      =  (word . pack . show) w
instance (Annotation t) => PP (Expression t) where
  pp (Literal lit)           =  word (Esc.bytes lit)
  pp (UnescapedLiteral u)    =  word u
  pp Asterisk                =  word "*"
  pp QuestionMark            =  word "?"
  pp Tilde                   =  word "~"
  pp (ReadVar var) = (word . quote) (if s == "$!" then "${!}" else s)
   where -- Need to be careful to avoid history expansion.
    s                        =  (('$' `cons`) . identpart) var
  pp (ReadVarSafe var)       =  (word . quote . braces0 . identpart) var
  pp (ReadArray ident expr)  =  (word . quote . braces)
                                (bytes ident `append` brackets (bytes expr))
  pp (ReadArraySafe ident expr) = (word . quote . braces0)
                                  (bytes ident `append` brackets (bytes expr))
  -- Examples that all work for nasty arguments containing brackets:
  --   echo "${array[$1]}"
  --   echo "${array["$1"]}"
  --   echo "${array["$1""$2"]}"
  -- Looks like we can get away with murder here.
  pp (ARGVElements)          =  word "\"$@\""
  pp (ARGVLength)            =  word "$#"
  pp (Elements ident)        =  (word . quote . braces)
                                (bytes ident `append` "[@]")
  pp (ElementsSafe ident)    =  (word . quote . braces_)
                                (bytes ident `append` "[@]")
  pp (Keys ident)            =  (word . quote . braces)
                                ('!' `cons` bytes ident `append` "[@]")
  pp (Length ident)          =  (word . quote . braces)
                                ('#' `cons` identpart ident)
  pp (Trim trim var expr)    =  (word . quote . braces . mconcat)
                                [identpart var, trimPrinter trim, bytes expr]
  pp (ArrayLength ident)     =  (word . quote . braces)
                                ('#' `cons` bytes ident `append` "[@]")
  pp (Concat expr0 expr1)    =  wordcat [bytes expr0, bytes expr1]
  pp (Eval ann)              =  inlineEvalPrinter "\"$(" ")\"" ann
  pp (EvalUnquoted ann)      =  inlineEvalPrinter "$(" ")" ann
  pp (ProcessIn ann)         =  inlineEvalPrinter "<(" ")" ann
  pp (ProcessOut ann)        =  inlineEvalPrinter ">(" ")" ann
instance (Annotation t) => PP (Annotated t) where
  pp (Annotated t stmt)      =  annotate t stmt
instance (Annotation t) => PP (Statement t) where
  pp term                    =  case term of
    Empty                   ->  return ()
    SimpleCommand cmd args  ->  do hangMultiline cmd
                                   mapM_ breakline args
                                   outdent
    NoOp msg | null msg     ->  word ":"
             | otherwise    ->  word ":" >> (word . escapeWords) msg
    Bang t                  ->  hangWord "!" >> binGrp t >> outdent
    AndAnd t t'             ->  if isSimple t && (isSimple t' || isAndAnd t')
                                then pp t     >> word "&&" >> nl >> pp t'
                                else binGrp t >> word "&&" >> nl >> binGrp t'
    OrOr t t'               ->  if isSimple t && (isSimple t' || isOrOr t')
                                then pp t     >> word "||" >> nl >> pp t'
                                else binGrp t >> word "||" >> nl >> binGrp t'
    Pipe t t'               ->  if isSimple t && (isSimple t' || isPipe t')
                                then pp t     >> word "|"  >> nl >> pp t'
                                else binGrp t >> word "|"  >> nl >> binGrp t'
    Sequence t t'           ->  pp t     >> nl        >> pp t'
    Background t t'         ->  binGrp t >> word "&"  >> nl >> pp t'
    Group t                 ->  curlyOpen >> pp t     >> curlyClose >> outdent
    Subshell t              ->  roundOpen >> pp t     >> roundClose >> outdent
    Function fname t        ->  do wordcat ["function ", bytes fname]
                                   inword "{" >> pp t >> outword "}"
    IfThen t t'             ->  do hangWord "if"   >> pp t  >> outdent >> nl
                                   inword   "then" >> pp t'
                                   outword  "fi"
    IfThenElse t t' t''     ->  do hangWord "if"   >> pp t  >> outdent >> nl
                                   inword   "then" >> pp t' >> outdent >> nl
                                   inword   "else" >> pp t''
                                   outword  "fi"
    For var vals t          ->  do hangWord (concat ["for ", bytes var, " in"])
                                   mapM_ breakline vals
                                   outdent >> nl
                                   inword "do" >> pp t >> outword "done"
    Case expr cases         ->  do word "case" >> pp expr >> inword "in"
                                   mapM_ case_clause cases
                                   outword "esac"
    While t t'              ->  do hangWord "while" >> pp t >> outdent >> nl
                                   inword "do" >> pp t' >> outword "done"
    Until t t'              ->  do hangWord "until" >> pp t >> outdent >> nl
                                   inword "do" >> pp t' >> outword "done"
--  BraceBrace _            ->  error "[[ ]]"
    Assign (Var var val)    ->  do hang (bytes var `mappend` "=")
                                   pp val >> outdent
    Assign (Array var exps) ->  do hangcat [bytes var, "=("]
                                   array_pp pp exps >> word ")"
                                   nl >> outdent
    Assign (Dict var pairs) ->  do hangcat [bytes var, "=("]
                                   array_pp keyset pairs
                                   nl >> outdent >> word ")"
    Declare (Var var val)   ->  do hang $ concat ["declare ", bytes var, "="]
                                   pp val >> outdent
    Declare (Array var exps) -> do hangcat ["declare -a ", bytes var, "=("]
                                   array_pp pp exps >> word ")"
                                   nl >> outdent
    Declare (Dict var pairs) -> do hangcat ["declare -A ", bytes var, "=("]
                                   array_pp keyset pairs >> word ")"
                                   nl >> outdent
    Local (Var var val)     ->  do hang $ concat ["local ", bytes var, "="]
                                   pp val >> outdent
    Local (Array var exps)  ->  do hangcat ["local -a ", bytes var, "=("]
                                   array_pp pp exps >> word ")"
                                   nl >> outdent
    Local (Dict var pairs)  ->  do hangcat ["local -A ", bytes var, "=("]
                                   array_pp keyset pairs >> word ")"
                                   nl >> outdent
    Export var val          ->  do hangcat ["export ", bytes var, "="]
                                   pp val >> outdent
    IsSet var               ->  wordcat ["[[ ${",identpart var,":+true} ]]"]
    ArrayUpdate var key val ->  pp (DictUpdate var key val)
    DictUpdate var key val  ->  wordcat
                                [bytes var, "[", bytes key, "]=", bytes val]
    Redirect stmt d fd t    ->  do redirectGrp stmt
                                   word (render_redirect d fd t)

hangcat                      =  hangWord . concat

array_pp _   [   ]           =  return ()
array_pp ppF (h:t)           =  ppF h >> mapM_ ppFNL t
 where
  ppFNL x                    =  nl >> ppF x

keyset (key, val)            =  wordcat ["[", bytes key, "]=", bytes val]

case_clause (ptrn, stmt)     =  do hangWord (bytes ptrn `append` ") ")
                                   pp stmt >> word ";;" >> outdent >> nl

render_redirect direction fd target =
  concat [ bytes fd, case direction of In     -> "<"
                                       Out    -> ">"
                                       Append -> ">>"
                   , case target of Left expr -> bytes expr
                                    Right fd' -> '&' `cons` bytes fd' ]

quote b                      =  cons '"' b `snoc` '"'

braces b                     =  "${" `append` b `snoc` '}'

braces0 b                    =  "${" `append` b `append` ":-}"

braces_ b                    =  concat ["${", b, ":+${", b, "}}"]

brackets b                   =  cons '[' b `snoc` ']'

identpart (VarSpecial special) = (drop 1 . bytes) special
identpart (VarIdent ident)   =  bytes ident

trimPrinter                 ::  Trim -> ByteString
trimPrinter ShortestLeading  =  "#"
trimPrinter LongestLeading   =  "##"
trimPrinter ShortestTrailing =  "%"
trimPrinter LongestTrailing  =  "%%"

isSimple (Annotated _ (SimpleCommand _ _)) = True
isSimple _                                 = False

isAndAnd (Annotated _ (AndAnd _ _)) = True
isAndAnd _                          = False

isOrOr (Annotated _ (OrOr _ _)) = True
isOrOr _                        = False

isPipe (Annotated _ (Pipe _ _)) = True
isPipe _                        = False

binGrp a@(Annotated _ stmt)  =  case stmt of
  Bang _                    ->  curlyOpen >> pp a >> curlyClose
  AndAnd _ _                ->  curlyOpen >> pp a >> curlyClose
  OrOr _ _                  ->  curlyOpen >> pp a >> curlyClose
  Pipe _ _                  ->  curlyOpen >> pp a >> curlyClose
  Sequence _ _              ->  curlyOpen >> pp a >> curlyClose
  Background _ _            ->  curlyOpen >> pp a >> curlyClose
  _                         ->  pp a

redirectGrp a@(Annotated _ stmt) = case stmt of
  Redirect _ _ _ _          ->  curlyOpen >> pp a >> curlyClose
  _                         ->  binGrp a

breakline                   ::  (PP t) => t -> State PPState ()
breakline printable          =  do
  PPState{..}               <-  get
  when (columns + maxLineLength printed + 1 > 79 && columns /= sum indents)
       (opM [Bytes "\\", Newline])
  pp printable
 where
  printed                    =  bytes printable

hangMultiline printable      =  do
  pp printable
  opM [Indent (finalLineLength printed + 1)]
 where
  printed                    =  bytes printable

maxLineLength = fromIntegral . List.foldl' max 0 . fmap length . lines

finalLineLength b            =  case lines b of
  [ ]                       ->  0
  h:t                       ->  (fromIntegral . length . List.last) (h:t)

inlineEvalPrinter open close ann = do
  indentPadToNextWord
  hangWord open
  pp ann
  word close
  outdent >> outdent

escapeWords s = unwords ((Esc.bytes . Esc.bash) <$> words s)


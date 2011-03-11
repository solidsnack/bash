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

import qualified Data.List as List
import Data.Word (Word8)
import Data.ByteString.Char8
import Data.Binary.Builder (Builder)
import Prelude hiding (concat, length, replicate, lines, drop, null)
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
instance PP SpecialVar where
  pp                         =  word . specialVarBytes
instance PP FileDescriptor where
  pp (FileDescriptor w)      =  (word . pack . show) w
instance (Annotation t) => PP (Expression t) where
  pp (Literal lit)           =  word (Esc.bytes lit)
  pp Asterisk                =  word "*"
  pp QuestionMark            =  word "?"
  pp Tilde                   =  word "~"
  pp (ReadVar var)           =  (word . quote . ('$' `cons`) . identpart) var
  pp (ReadVarSafe var)       =  (word . quote . braces0 . identpart) var
  pp (ReadArray ident expr)  =  (word . braces)
                                (bytes ident `append` brackets (bytes expr))
  pp (ReadArraySafe ident expr) = (word . braces0)
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
  pp (Length ident)          =  (word . quote . braces)
                                ('#' `cons` identpart ident)
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
    SimpleCommand cmd args  ->  do hangMultiline cmd
                                   mapM_ breakline args
                                   outdent
    NoOp msg | null msg     ->  word ":"
             | otherwise    ->  word ":" >> (word . Esc.bytes . Esc.bash) msg
    Bang t                  ->  hang "!"      >> binGrp t >> outdent
    AndAnd t t'             ->  binGrp t >> word "&&" >> nl >> binGrp t'
    OrOr t t'               ->  binGrp t >> word "||" >> nl >> binGrp t'
    Pipe t t'               ->  binGrp t >> word "|"  >> nl >> binGrp t'
    Sequence t t'           ->  pp t          >> nl        >> pp t'
    Background t t'         ->  binGrp t >> word "&"  >> nl >> pp t'
    Group t                 ->  curlyOpen >> pp t     >> curlyClose >> outdent
    Subshell t              ->  roundOpen >> pp t     >> roundClose >> outdent
    Function ident t        ->  do wordcat ["function ", bytes ident]
                                   inword " {" >> pp t >> outword "}"
    IfThen t t'             ->  do hang "if" >> pp t   >> outdent   >> nl
                                   inword "then" >> pp t' >> outword "fi"
    IfThenElse t t' t''     ->  do hang "if" >> pp t   >> outdent   >> nl
                                   inword "then"       >> pp t'     >> outdent
                                   nl
                                   inword "else"       >> pp t''
                                   outword "fi"
    For var vals t          ->  do hang (concat ["for ", bytes var, " in"])
                                   mapM_ breakline vals
                                   outdent >> nl
                                   inword "do" >> pp t >> outword "done"
    Case expr cases         ->  do word "case" >> pp expr >> inword "in"
                                   mapM_ case_clause cases
                                   outword "esac"
    While t t'              ->  do hang "while" >> pp t >> outdent >> nl
                                   inword "do" >> pp t' >> outword "done"
    Until t t'              ->  do hang "until" >> pp t >> outdent >> nl
                                   inword "do" >> pp t' >> outword "done"
--  BraceBrace _            ->  error "[[ ]]"
    VarAssign var val       ->  pp var >> word "=" >> pp val
    ArrayDecl var exprs     ->  do hangcat ["declare -a ", bytes var, "=("]
                                   array_pp pp exprs >> word ")"
                                   nl >> outdent
    ArrayUpdate var key val ->  pp (DictUpdate var key val)
    ArrayAssign var exprs   ->  do hangcat [bytes var, "=("]
                                   array_pp pp exprs >> word ")"
                                   nl >> outdent
    DictDecl var pairs      ->  do hangcat ["declare -A ", bytes var, "=("]
                                   array_pp keyset pairs >> word ")"
                                   nl >> outdent
    DictUpdate var key val  ->  do hangcat [bytes var, "[", bytes key, "]="]
                                   pp val >> outdent
    DictAssign var pairs    ->  do hangcat [bytes var, "=("]
                                   array_pp keyset pairs
                                   nl >> outdent >> word ")"
    Redirect stmt d fd t    ->  do redirectGrp stmt
                                   word (render_redirect d fd t)

hangcat                      =  hang . concat

array_pp ppF [   ]           =  return ()
array_pp ppF (h:t)           =  ppF h >> mapM_ ppFNL t
 where
  ppFNL x                    =  nl >> ppF x

keyset (key, val) = word "[" >> pp key >> word "]=" >> pp val

case_clause (ptrn, stmt)     =  do hang (bytes ptrn `append` ") ")
                                   pp stmt >> word ";;" >> outdent >> nl

render_redirect direction fd target =
  concat [ bytes fd, case direction of In     -> "<"
                                       Out    -> ">"
                                       Append -> ">>"
                   , case target of Left expr -> bytes expr
                                    Right fd' -> '&' `cons` bytes fd' ]

quote b                      =  '"' `cons` b `snoc` '"'

braces b                     =  "${" `append` b `snoc` '}'

braces0 b                    =  "${" `append` b `append` ":-}"

brackets b                   =  '[' `cons` b `snoc` ']'

identpart (Left special)     =  (drop 1 . bytes) special
identpart (Right ident)      =  bytes ident

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
       (opM [Word "\\", Newline])
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

inlineEvalPrinter open close ann =  do
  indentPadToNextWord
  hang open
  pp ann
  word close
  outdent >> outdent



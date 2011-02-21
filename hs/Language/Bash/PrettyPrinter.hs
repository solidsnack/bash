{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , RecordWildCards
           , NamedFieldPuns
           , NoMonomorphismRestriction
           , GeneralizedNewtypeDeriving
  #-}

{-| Pretty printer for Bash. The pretty printer generates a builder which we
    pass to the "linker" later to put between the two main chunks of code.
 -}

module Language.Bash.PrettyPrinter where

import Data.Word (Word8)
import Data.ByteString.Char8
import Prelude hiding (concat, length, replicate, lines, drop, null)
import Control.Monad.State.Strict

import qualified Text.ShellEscape as Esc

import Language.Bash.Syntax
import Language.Bash.PrettyPrinter.State


class PP t where
  pp                        ::  t -> State PPState ()
instance PP Identifier where
  pp (Identifier b)          =  word b
instance PP SpecialVar where
  pp                         =  word . specialVarBytes
instance PP Expression where
  pp (Literal lit)           =  word (Esc.bytes lit)
  pp Asterisk                =  word "*"
  pp QuestionMark            =  word "?"
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
                                ('#' `cons` bytes ident)
  pp (ArrayLength ident)     =  (word . quote . braces)
                                ('#' `cons` bytes ident `append` "[@]")
  pp (Concat expr0 expr1)    =  wordcat [bytes expr0, bytes expr1]
instance PP FileDescriptor where
  pp (FileDescriptor w)      =  (word . pack . show) w

bytes                       ::  (PP t) => t -> ByteString
bytes                        =  renderBytes (nlCol 0) . pp

bytes_state                  =  renderBytes (nlCol 0)

{-  
nl                          ::  State PPState ()
hang                        ::  ByteString -> State PPState ()
word                        ::  ByteString -> State PPState ()
wordcat                     ::  [ByteString] -> State PPState ()
outdent                     ::  State PPState ()
inword                      ::  ByteString -> State PPState ()
outword                     ::  ByteString -> State PPState ()
arrayset                    ::  (ByteString, ByteString) -> State PPState ()
breakline                   ::  ByteString -> State PPState ()
 -}
instance PP Statement where
  pp term                    =  case term of
    SimpleCommand cmd args  ->  do hang (bytes cmd)
                                   mapM_ (breakline . bytes) args
                                   outdent
    NoOp msg | null msg     ->  word ":"
             | otherwise    ->  word ":" >> (word . Esc.bytes . Esc.bash) msg
    Bang t                  ->  hang "!"      >> pp (binGrp t) >> outdent
    AndAnd t t'             ->  do pp (binGrp t) >> word "&&"
                                   nl >> pp (binGrp t')
    OrOr t t'               ->  do pp (binGrp t) >> word "||"
                                   nl >> pp (binGrp t')
    Pipe t t'               ->  do pp (binGrp t) >> word "|"
                                   nl >> pp (binGrp t')
    Sequence t t'           ->  pp t          >> nl        >> pp t'
    Background t t'         ->  pp (binGrp t) >> word "&"  >> nl >> pp t'
    Group t                 ->  hang "{"  >> pp t      >> word ";}" >> outdent
    Subshell t              ->  hang "("  >> pp t      >> word ")"  >> outdent
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
                                   mapM_ (breakline . bytes) vals 
                                   outdent >> nl
                                   inword "do" >> pp t >> outword "done"
    Case expr cases         ->  do word "case" >> pp expr >> inword "in"
                                   mapM_ case_clause cases
                                   outword "esac"
    While t t'              ->  do hang "while" >> pp t >> outdent >> nl
                                   inword "do" >> pp t' >> outword "done"
    Until t t'              ->  do hang "until" >> pp t >> outdent >> nl
                                   inword "do" >> pp t' >> outword "done"
    BraceBrace _            ->  error "[[ ]]"
    VarAssign var val       ->  pp var >> word "=" >> pp val
    DictDecl var pairs      ->  do wordcat ["declare -A ", bytes var, "=("]
                                   nl >> mapM_ arrayset pairs
                                   nl >> word ")"
    DictUpdate var key val  ->  do pp var >> word "["
                                   pp key >> word "]="
                                   pp val
    DictAssign var pairs    ->  do pp var >> word "=(" >> nl
                                   mapM_ arrayset pairs >> word ")"
    Redirect stmt d fd t    ->  do pp (redirectGrp stmt)
                                   word (render_redirect d fd t)

arrayset (key, val) = word "[" >> pp key >> word "]=" >> pp val >> nl

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

binGrp t                     =  case t of
  Bang _                    ->  Group t
  AndAnd _ _                ->  Group t
  OrOr _ _                  ->  Group t
  Pipe _ _                  ->  Group t
  Sequence _ _              ->  Group t
  Background _ _            ->  Group t
  _                         ->  t

redirectGrp t                =  case t of
  Redirect _ _ _ _          ->  Group t
  _                         ->  binGrp t


{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , RecordWildCards
           , NamedFieldPuns
           , NoMonomorphismRestriction
  #-}

{-| Pretty printer for Bash. The pretty printer generates a builder which we
    pass to the "linker" later to put between the two main chunks of code.
 -}

module Language.Bash.PrettyPrinter where

import Data.Word (Word8)
import Data.ByteString.Char8 (ByteString, concat, pack, cons, lines)
import Prelude hiding (concat, length, replicate, lines)
import Control.Monad.State.Strict

import qualified Text.ShellEscape as Esc

import Language.Bash.Syntax
import Language.Bash.PrettyPrinter.State


class PP t where
  pp                        ::  t -> State PPState ()
instance PP Identifier where
  pp (Identifier b)          =  word b
instance PP Expression where
  pp (Literal lit)           =  word (Esc.bytes lit)
  pp Asterisk                =  word "*"
  pp QuestionMark            =  word "?"
  pp (ReadVar ident)         =  word "$" >> pp ident
  pp (ReadVarSafe ident)     =  word "${" >> pp ident >> word ":-}"
  pp (ReadArray ident expr)  =  do word "${" >> pp ident
                                   word "[" >> pp expr >> word "]}"
  pp (ReadArraySafe ident expr) = do word "${" >> pp ident
                                     word "[" >> pp expr >> word "]:-}"
  -- Examples that all work for nasty arguments containing brackets:
  --   echo "${array[$1]}"
  --   echo "${array["$1"]}"
  --   echo "${array["$1""$2"]}"
  -- Looks like we can get away with murder here.
  pp (ARGVElements)          =  word "\"$@\""
  pp (ARGVLength)            =  word "$#"
  pp (Elements ident)        =  word "\"${" >> pp ident >> word "[@]}\""
  pp (Length ident)          =  word "${#" >> pp ident >> word "}"
  pp (ArrayLength ident)     =  word "${#" >> pp ident >> word "[@]}"
  pp (Concat expr0 expr1)    =  case (expr0, expr1) of
    (ReadVar _, Literal _)  ->  word "\"" >> pp expr0 >> word "\"" >> pp expr1
    _                       ->  pp expr0 >> pp expr1
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
    NoOp                    ->  word ": 'Do nothing.'"
    Raw b                   ->  mapM_ word (lines b)
    Bang t                  ->  hang "!"  >> pp t      >> outdent
    AndAnd t t'             ->  pp t      >> word "&&" >> nl        >> pp t'
    OrOr t t'               ->  pp t      >> word "||" >> nl        >> pp t'
    Pipe t t'               ->  pp t      >> word "|"  >> nl        >> pp t'
    Sequence t t'           ->  pp t                   >> nl        >> pp t'
    Background t t'         ->  pp t      >> word "&"  >> nl        >> pp t'
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
    Redirect stmt d fd t    ->  pp stmt >> word (render_redirect d fd t) >> nl

arrayset (key, val) = word "[" >> pp key >> word "]=" >> pp val >> nl

case_clause (ptrn, stmt)     =  do hang (bytes_state (pp ptrn >> word ")  "))
                                   pp stmt >> word ";;" >> nl

render_redirect direction fd target =
  concat [ bytes fd, case direction of In     -> "<"
                                       Out    -> ">"
                                       Append -> ">>"
                   , case target of Left expr -> bytes expr
                                    Right fd' -> '&' `cons` bytes fd' ]



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

import Data.ByteString.Char8 (ByteString, concat)
import Prelude hiding (concat, length, replicate)
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

bytes                       ::  (PP t) => t -> ByteString
bytes                        =  renderBytes (nlCol 0) . pp

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
ops                         ::  Statement -> State PPState ()
ops term                     =  case term of
  SimpleCommand cmd args    ->  do hang (bytes cmd)
                                   mapM_ (breakline . bytes) args
                                   outdent
  NoOp                      ->  word ": 'Do nothing.'"
  Bang t                    ->  hang "!"  >> ops t     >> outdent
  AndAnd t t'               ->  ops t     >> word "&&" >> nl        >> ops t'
  OrOr t t'                 ->  ops t     >> word "||" >> nl        >> ops t'
  Pipe t t'                 ->  ops t     >> word "|"  >> nl        >> ops t'
  Sequence t t'             ->  ops t                  >> nl        >> ops t'
  Background t t'           ->  ops t     >> word "&"  >> nl        >> ops t'
  Group t                   ->  hang "{"  >> ops t     >> word ";}" >> outdent
  Subshell t                ->  hang "("  >> ops t     >> word ")"  >> outdent
  Function ident t          ->  do wordcat ["function ", bytes ident]
                                   inword " {" >> ops t >> outword "}"
  IfThen t t'               ->  do hang "if" >> ops t >> outdent >> nl
                                   inword "then" >> ops t' >> outword "fi"
  IfThenElse t t' t''       ->  do hang "if" >> ops t >> outdent >> nl
                                   inword "then"      >> ops t'  >> outdent
                                   inword "else"      >> ops t''
                                   outword "fi"
  For var vals t            ->  do hang (concat ["for ", bytes var, " in"])
                                   mapM_ (breakline . bytes) vals 
                                   outdent >> nl
                                   inword "do" >> ops t >> outword "done"
  BraceBrace _              ->  error "[[ ]]"
  VarAssign var val         ->  pp var >> word "=" >> pp val
  DictDecl var pairs        ->  do wordcat ["declare -A ", bytes var, "=("]
                                   nl >> mapM_ arrayset pairs
                                   nl >> word ")"
  DictUpdate var key val    ->  do pp var >> word "["
                                   pp key >> word "]="
                                   pp val
  DictAssign var pairs      ->  do pp var >> word "=(" >> nl
                                   mapM_ arrayset pairs >> word ")"

arrayset                    ::  (PP s, PP t) => (s, t) -> State PPState ()
arrayset (key, val) = word "[" >> pp key >> word "]=" >> pp val >> nl

{-


  = SimpleCommand   Expression          [Expression]
  | NoOp
  | Bang            Statement
  | AndAnd          Statement           Statement
  | OrOr            Statement           Statement
  | Pipe            Statement           Statement
  | Sequence        Statement           Statement
  | Background      Statement           Statement
  | Group           Statement
  | Subshell        Statement
  | Function        Identifier          Statement
  | IfThen          Statement           Statement
  | IfThenElse      Statement           Statement           Statement
  | For             Identifier          [Expression]        Statement
  | Case            Expression          [(Expression, Statement)]
  | While           Statement           Statement
  | Until           Statement           Statement
  | BraceBrace      ConditionalExpression
  | VarAssign       Identifier          Expression
  | DictDecl        Identifier          [(Identifier, Expression)]
  | DictUpdate      Identifier          Expression          Expression
  | DictAssign      Identifier          [(Expression, Expression)]

 -}

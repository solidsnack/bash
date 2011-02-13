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


-- builder                     ::  PPState -> Statement -> Builder
-- builder init t               =  string $ execState (ops t) init


-- ops                         ::  Statement -> State PPState ()
-- ops term                     =  case term of
--   SimpleCommand cmd args    ->  hang cmd >> mapM_ breakline vals >> outdent
--   Empty                     ->  word ": 'Do nothing.'"
--   Bang t                    ->  hang "!" >> ops t >> outdent
--   And t t'                  ->  ops t >> word "&&" >> nl >> ops t'
--   Or t t'                   ->  ops t >> word "||" >> nl >> ops t'
--   Pipe t t'                 ->  ops t >> word "|"  >> nl >> ops t'
--   Sequence t t'             ->  ops t              >> nl >> ops t'
--   Background t t'           ->  ops t >> word "&"  >> nl >> ops t'
--   Group t                   ->  hang "{"  >> ops t >> word ";}" >> outdent
--   Subshell t                ->  hang "("  >> ops t >> word ")"  >> outdent
--   Function b t              ->  wordcat ["function ", b]
--                             >>  inword " {" >> ops t >> outword "}"
--   IfThen t t'               ->  hang "if" >> ops t >> outdent >> nl
--                             >>  inword "then"      >> ops t'  >> outword "fi"
--   IfThenElse t t' t''       ->  hang "if" >> ops t >> outdent >> nl
--                             >>  inword "then"      >> ops t'  >> outdent
--                             >>  inword "else"      >> ops t'' >> outword "fi"
--   ForDoDone var vals t      ->  hang (concat ["for ", var, " in"])
--                             >>  mapM_ breakline vals
--                             >>  outdent >> nl
--                             >>  inword "do" >> ops t >> outword "done"
--   VarAssign var val         ->  wordcat [var, "=", val]
--   DictDecl var pairs        ->  wordcat ["declare -A ", var, "=("] >> nl
--                             >>  mapM_ (opM .  arrayset) pairs
--                             >>  nl >> word ")"
--   DictUpdate var key val    ->  wordcat [var, "[", key, "]=", val]
--   DictAssign var pairs      ->  wordcat [var, "=("] >> nl
--                             >>  mapM_ (opM .  arrayset) pairs >> word ")"


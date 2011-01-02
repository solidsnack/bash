{-# LANGUAGE EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
  #-}

module Language.Bash.Syntax where

import Prelude hiding (all)
import Data.Char
import Data.String
import Data.Maybe
import Data.ByteString.Char8

import qualified Text.ShellEscape as Esc


data Statement
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
  | DictDecl        Identifier          [(Expression, Expression)]
  | DictUpdate      Identifier          Expression          Expression
  | DictAssign      Identifier          [(Expression, Expression)]
deriving instance Eq Statement
deriving instance Ord Statement
deriving instance Show Statement


cmd                         ::  ByteString -> [ByteString] -> Statement
cmd argv0 argv               =  SimpleCommand (e argv0) (fmap e argv)
 where
  e                          =  Literal . Esc.bash

data Expression              =  Literal Esc.Bash
                             |  ReadVar Identifier
                             |  ReadArray Identifier Expression
                             |  IndirectExpansion Identifier
                             |  Concat Expression Expression
                             |  Exec Statement
-- Ignore                    |  crazy patterns
deriving instance Eq Expression
deriving instance Ord Expression
deriving instance Show Expression
instance IsString Expression where
  fromString                 =  Literal . Esc.bash . fromString


newtype Identifier           =  Identifier ByteString
deriving instance Eq Identifier
deriving instance Ord Identifier
deriving instance Show Identifier
instance IsString Identifier where
  fromString                 =  fromJust . identifier . fromString

identifier                  ::  ByteString -> Maybe Identifier
identifier bytes             =  do
  (c, bytes')               <-  uncons bytes
  if okayHead c && all okayTail bytes'
    then  Just (Identifier bytes)
    else  Nothing
 where
  okayTail c                 =  (isAlphaNum c || c == '_') && isAscii c
  okayHead c                 =  (isAlpha c || c == '_') && isAscii c


data ConditionalExpression
  = File_a          Expression
  | File_b          Expression
  | File_c          Expression
  | File_d          Expression
  | File_e          Expression
  | File_f          Expression
  | File_g          Expression
  | File_h          Expression
  | File_k          Expression
  | File_p          Expression
  | File_r          Expression
  | File_s          Expression
  | File_t          Expression
  | File_u          Expression
  | File_w          Expression
  | File_x          Expression
  | File_O          Expression
  | File_G          Expression
  | File_L          Expression
  | File_S          Expression
  | File_N          Expression
  | File_nt         Expression          Expression
  | File_ot         Expression          Expression
  | File_ef         Expression          Expression
  | OptSet          Expression
  | StringEmpty     Expression
  | StringNonempty  Expression
  | StringEq        Expression          Expression
  | StringNotEq     Expression          Expression
  | StringLT        Expression          Expression
  | StringGT        Expression          Expression
  | StringRE        Expression          Expression
  | NumEq           Expression          Expression
  | NumNotEq        Expression          Expression
  | NumLT           Expression          Expression
  | NumLE           Expression          Expression
  | NumGT           Expression          Expression
  | NumGE           Expression          Expression
  | Not             Expression          Expression
  | And             Expression          Expression
  | Or              Expression          Expression
deriving instance Eq ConditionalExpression
deriving instance Ord ConditionalExpression
deriving instance Show ConditionalExpression


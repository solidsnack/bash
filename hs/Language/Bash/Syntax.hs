{-# LANGUAGE EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
  #-}

module Language.Bash.Syntax where

import Prelude hiding (all)
import Data.Char
import Data.String
import Data.Maybe
import Data.Word (Word8)
import Data.ByteString.Char8

import qualified Text.ShellEscape as Esc


data Statement
  = SimpleCommand   Expression          [Expression]
  | NoOp
  | Raw             ByteString
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
  | Redirect        Statement           Redirection
                    FileDescriptor      (Either Expression FileDescriptor)
deriving instance Eq Statement
deriving instance Ord Statement
deriving instance Show Statement


cmd                         ::  ByteString -> [ByteString] -> Statement
cmd argv0 argv               =  SimpleCommand (e argv0) (fmap e argv)
 where
  e                          =  Literal . Esc.bash

data Expression              =  Literal Esc.Bash
                             |  Asterisk
                             |  QuestionMark
                             |  ReadVar (Either SpecialVar Identifier)
                             |  ReadVarSafe (Either SpecialVar Identifier)
                             |  ReadArray Identifier Expression
                             |  ReadArraySafe Identifier Expression
                             |  ARGVElements
                             |  ARGVLength
                             |  Elements Identifier
                             |  Length Identifier
                             |  ArrayLength Identifier
                             |  Concat Expression Expression
-- TODO                      |  Exec Statement
-- TODO                      |  IndirectExpansion Identifier
-- TODO                      |  Substring, Replacement, &c.
-- TODO                      |  ProcessSubstituion
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


newtype FileDescriptor       =  FileDescriptor Word8
deriving instance Eq FileDescriptor
deriving instance Ord FileDescriptor
deriving instance Num FileDescriptor
deriving instance Show FileDescriptor


data Redirection             =  In | Out | Append
deriving instance Eq Redirection
deriving instance Ord Redirection
deriving instance Show Redirection


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

data SpecialVar
  = DollarQuestion | Dollar0 | Dollar1 | Dollar2 | Dollar3 | Dollar4
                   | Dollar5 | Dollar6 | Dollar7 | Dollar8 | Dollar9
deriving instance Eq SpecialVar
deriving instance Ord SpecialVar
deriving instance Show SpecialVar
instance IsString SpecialVar where
  fromString                 =  fromJust . specialVar . fromString

specialVar                  ::  ByteString -> Maybe SpecialVar
specialVar b | "$?" == b     =  Just DollarQuestion
             | "$0" == b     =  Just Dollar0
             | "$1" == b     =  Just Dollar1
             | "$2" == b     =  Just Dollar2
             | "$3" == b     =  Just Dollar3
             | "$4" == b     =  Just Dollar4
             | "$5" == b     =  Just Dollar5
             | "$6" == b     =  Just Dollar6
             | "$7" == b     =  Just Dollar7
             | "$8" == b     =  Just Dollar8
             | "$9" == b     =  Just Dollar9
             | otherwise     =  Nothing

specialVarBytes             ::  SpecialVar -> ByteString
specialVarBytes DollarQuestion = "$?"
specialVarBytes Dollar0      =  "$0"
specialVarBytes Dollar1      =  "$1"
specialVarBytes Dollar2      =  "$2"
specialVarBytes Dollar3      =  "$3"
specialVarBytes Dollar4      =  "$4"
specialVarBytes Dollar5      =  "$5"
specialVarBytes Dollar6      =  "$6"
specialVarBytes Dollar7      =  "$7"
specialVarBytes Dollar8      =  "$8"
specialVarBytes Dollar9      =  "$9"


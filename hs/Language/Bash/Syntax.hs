{-# LANGUAGE EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
           , GeneralizedNewtypeDeriving
           , NoMonomorphismRestriction
  #-}
{-| Bash statements and expressions. The statement tree is a functor,
    supporting arbitrary annotations; this is intended to support analysis of
    effects and privilege levels as well as commenting and arbitrary code
    inclusion.
 -}
module Language.Bash.Syntax where

import Prelude hiding (all)
import Control.Arrow ((***))
import Data.Char
import Data.String
import Data.Maybe
import Data.Word (Word8)
import Data.ByteString.Char8
import Data.Foldable hiding (all)
import Data.Monoid

import qualified Text.ShellEscape as Esc


{-| The 'Annotated' type captures the annotatedness of a tree of Bash
    statements.
 -}
data Annotated               =  Annotated { before    :: [ByteString]
                                          , statement :: Statement
                                          , after     :: [ByteString] }
deriving instance Eq Annotated
deriving instance Ord Annotated
deriving instance Show Annotated

{-| The 'Statement' type captures the different kind of statements that may
    exist in a Bash statement tree. It is mutually recursive with 'Annotated'.
 -}
data Statement
  = SimpleCommand   Expression          [Expression]
  | NoOp            ByteString
  | Bang            Annotated
  | AndAnd          Annotated           Annotated
  | OrOr            Annotated           Annotated
  | Pipe            Annotated           Annotated
  | Sequence        Annotated           Annotated
  | Background      Annotated           Annotated
  | Group           Annotated
  | Subshell        Annotated
  | Function        Identifier          Annotated
  | IfThen          Annotated           Annotated
  | IfThenElse      Annotated           Annotated           Annotated
  | For             Identifier          [Expression]        Annotated
  | Case            Expression          [(Expression, Annotated)]
  | While           Annotated           Annotated
  | Until           Annotated           Annotated
--  BraceBrace      (ConditionalExpression)
  | VarAssign       Identifier          Expression
  | ArrayDecl       Identifier          [Expression]
  | ArrayUpdate     Identifier          Expression          Expression
  | ArrayAssign     Identifier          [Expression]
  | DictDecl        Identifier          [(Identifier, Expression)]
  | DictUpdate      Identifier          Expression          Expression
  | DictAssign      Identifier          [(Expression, Expression)]
  | Redirect        Annotated           Redirection
                    FileDescriptor      (Either Expression FileDescriptor)
deriving instance Eq Statement
deriving instance Ord Statement
deriving instance Show Statement

{-| The type of Bash expressions, handling many kinds of variable reference as
    well as eval and process substitution. It is 'Foldable' and a 'Functor'.
 -}
data Expression
   = Literal        Esc.Bash
   | Asterisk
   | QuestionMark
   | Tilde
   | ReadVar        (Either SpecialVar Identifier)
   | ReadVarSafe    (Either SpecialVar Identifier)
   | ReadArray      Identifier          Expression
   | ReadArraySafe  Identifier          Expression
   | ARGVElements
   | ARGVLength
   | Elements       Identifier
   | Keys           Identifier
   | Length         (Either SpecialVar Identifier)
   | ArrayLength    Identifier
   | Concat         Expression          Expression
   | Eval           Annotated
   | EvalUnquoted   Annotated
   | ProcessIn      Annotated
   | ProcessOut     Annotated
-- TODO                      |  IndirectExpansion Identifier
-- TODO                      |  Substring, Replacement, &c.
deriving instance Eq Expression
deriving instance Ord Expression
deriving instance Show Expression
instance IsString Expression where
  fromString                 =  literal . fromString

{-| Escape a 'ByteString' to produce a literal expression.
 -}
literal                     ::  ByteString -> Expression
literal                      =  Literal . Esc.bash

{-| The type of legal Bash identifiers, strings beginning with letters or @_@
    and containing letters, @_@ and digits.
 -}
newtype Identifier           =  Identifier ByteString
deriving instance Eq Identifier
deriving instance Ord Identifier
deriving instance Show Identifier
instance IsString Identifier where
  fromString                 =  fromJust . identifier . fromString

{-| Produce an 'Identifier' from a 'ByteString' of legal format.
 -}
identifier                  ::  ByteString -> Maybe Identifier
identifier bytes             =  do
  (c, bytes')               <-  uncons bytes
  if okayHead c && all okayTail bytes'
    then  Just (Identifier bytes)
    else  Nothing
 where
  okayTail c                 =  (isAlphaNum c || c == '_') && isAscii c
  okayHead c                 =  (isAlpha c || c == '_') && isAscii c

{-| A file descriptor in Bash is simply a number between 0 and 255.
 -}
newtype FileDescriptor       =  FileDescriptor Word8
deriving instance Eq FileDescriptor
deriving instance Ord FileDescriptor
deriving instance Num FileDescriptor
deriving instance Show FileDescriptor

{-| Redirection \"directions\".
 -}
data Redirection             =  In -- ^ Input redirection, @<@.
                             |  Out -- ^ Output redirection, @>@.
                             |  Append -- ^ Appending output, @>>@.
deriving instance Eq Redirection
deriving instance Ord Redirection
deriving instance Show Redirection

{-| Unused at present.
 -}
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
  | NumLEq          Expression          Expression
  | NumGT           Expression          Expression
  | NumGEq          Expression          Expression
  | Not             Expression          Expression
  | And             Expression          Expression
  | Or              Expression          Expression
deriving instance Eq ConditionalExpression
deriving instance Ord ConditionalExpression
deriving instance Show ConditionalExpression

{-| The names of special variables, with otherwise illegal identifiers, are
    represented by this type.
 -}
data SpecialVar
  = DollarQuestion | Dollar0 | Dollar1 | Dollar2 | Dollar3 | Dollar4
                   | Dollar5 | Dollar6 | Dollar7 | Dollar8 | Dollar9
deriving instance Eq SpecialVar
deriving instance Ord SpecialVar
deriving instance Show SpecialVar
instance IsString SpecialVar where
  fromString                 =  fromJust . specialVar . fromString

{-| Try to render a 'SpecialVar' from a 'ByteString'.
 -}
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


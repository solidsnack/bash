{-# LANGUAGE EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
           , GeneralizedNewtypeDeriving
  #-}

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


data Annotated t             =  Annotated t (Statement t)
deriving instance (Eq t) => Eq (Annotated t)
deriving instance (Ord t) => Ord (Annotated t)
deriving instance (Show t) => Show (Annotated t)
instance Functor Annotated where
  fmap f (Annotated t stmt)  =  Annotated (f t) (fmap f stmt)
instance Foldable Annotated where
  foldMap f (Annotated t stmt) = f t `mappend` foldMap f stmt

data Statement t
  = SimpleCommand   Expression          [Expression]
  | NoOp            ByteString
  | Bang            (Annotated t)
  | AndAnd          (Annotated t)       (Annotated t)
  | OrOr            (Annotated t)       (Annotated t)
  | Pipe            (Annotated t)       (Annotated t)
  | Sequence        (Annotated t)       (Annotated t)
  | Background      (Annotated t)       (Annotated t)
  | Group           (Annotated t)
  | Subshell        (Annotated t)
  | Function        Identifier          (Annotated t)
  | IfThen          (Annotated t)       (Annotated t)
  | IfThenElse      (Annotated t)       (Annotated t)       (Annotated t)
  | For             Identifier          [Expression]        (Annotated t)
  | Case            Expression          [(Expression, (Annotated t))]
  | While           (Annotated t)       (Annotated t)
  | Until           (Annotated t)       (Annotated t)
--  BraceBrace      ConditionalExpression
  | VarAssign       Identifier          Expression
  | DictDecl        Identifier          [(Identifier, Expression)]
  | DictUpdate      Identifier          Expression          Expression
  | DictAssign      Identifier          [(Expression, Expression)]
  | Redirect        (Annotated t)       Redirection
                    FileDescriptor      (Either Expression FileDescriptor)
deriving instance (Eq t) => Eq (Statement t)
deriving instance (Ord t) => Ord (Statement t)
deriving instance (Show t) => Show (Statement t)
instance Functor Statement where
  fmap f stmt                =  case stmt of
    SimpleCommand cmd args  ->  SimpleCommand cmd args
    NoOp b                  ->  NoOp b
    Bang ann                ->  Bang (fmap f ann)
    AndAnd ann ann'         ->  AndAnd (fmap f ann) (fmap f ann')
    OrOr ann ann'           ->  OrOr (fmap f ann) (fmap f ann')
    Pipe ann ann'           ->  Pipe (fmap f ann) (fmap f ann')
    Sequence ann ann'       ->  Sequence (fmap f ann) (fmap f ann')
    Background ann ann'     ->  Background (fmap f ann) (fmap f ann')
    Group ann               ->  Group (fmap f ann)
    Subshell ann            ->  Subshell (fmap f ann)
    Function id ann         ->  Function id (fmap f ann)
    IfThen ann ann'         ->  IfThen (fmap f ann) (fmap f ann')
    IfThenElse a a' a''     ->  IfThenElse (fmap f a) (fmap f a') (fmap f a'')
    For id args ann         ->  For id args (fmap f ann)
    Case expr cases         ->  Case expr (fmap (id *** fmap f) cases)
    While ann ann'          ->  While (fmap f ann) (fmap f ann')
    Until ann ann'          ->  Until (fmap f ann) (fmap f ann')
--  BraceBrace      ConditionalExpression
    VarAssign id expr       ->  VarAssign id expr
    DictDecl id assigns     ->  DictDecl id assigns
    DictUpdate id a b       ->  DictUpdate id a b
    DictAssign id assigns   ->  DictAssign id assigns
    Redirect ann r fd chan  ->  Redirect (fmap f ann) r fd chan
instance Foldable Statement where
  foldMap f stmt             =  case stmt of
    SimpleCommand _ _       ->  mempty
    NoOp _                  ->  mempty
    Bang ann                ->  foldMap f ann
    AndAnd ann ann'         ->  foldMap f ann `mappend` foldMap f ann'
    OrOr ann ann'           ->  foldMap f ann `mappend` foldMap f ann'
    Pipe ann ann'           ->  foldMap f ann `mappend` foldMap f ann'
    Sequence ann ann'       ->  foldMap f ann `mappend` foldMap f ann'
    Background ann ann'     ->  foldMap f ann `mappend` foldMap f ann'
    Group ann               ->  foldMap f ann
    Subshell ann            ->  foldMap f ann
    Function _ ann          ->  foldMap f ann
    IfThen ann ann'         ->  foldMap f ann `mappend` foldMap f ann'
    IfThenElse a a' a''     ->  (mconcat . fmap (foldMap f)) [a, a', a'']
    For _ _ ann             ->  foldMap f ann
    Case _ cases            ->  (mconcat . fmap (foldMap f . snd)) cases
    While ann ann'          ->  foldMap f ann `mappend` foldMap f ann'
    Until ann ann'          ->  foldMap f ann `mappend` foldMap f ann'
--  BraceBrace      ConditionalExpression
    VarAssign _ _           ->  mempty
    DictDecl _ _            ->  mempty
    DictUpdate _ _ _        ->  mempty
    DictAssign _ _          ->  mempty
    Redirect ann _ _ _      ->  foldMap f ann

cmd                         ::  ByteString -> [ByteString] -> Statement t
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
  fromString                 =  literal . fromString

literal                     ::  ByteString -> Expression
literal                      =  Literal . Esc.bash

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


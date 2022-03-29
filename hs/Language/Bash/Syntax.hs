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

import Prelude hiding (all, any, elem)
import Control.Applicative
import Control.Arrow ((***))
import Control.Monad
import Data.Char
import Data.String
import Data.Maybe
import Data.Word (Word8)
import Data.ByteString.Char8
import Data.Foldable hiding (all, any, elem)
import Data.Monoid

import qualified Text.ShellEscape as Esc


{-| The 'Annotated' type captures the annotatedness of a tree of Bash
    statements. It is 'Foldable' and a 'Functor'.
 -}
data Annotated t             =  Annotated { annotation :: t
                                          , statement :: Statement t }
deriving instance (Eq t) => Eq (Annotated t)
deriving instance (Ord t) => Ord (Annotated t)
deriving instance (Show t) => Show (Annotated t)
instance Functor Annotated where
  fmap f (Annotated t stmt)  =  Annotated (f t) (fmap f stmt)
instance Foldable Annotated where
  foldMap f (Annotated t stmt) = f t `mappend` foldMap f stmt

{-| The 'Statement' type captures the different kind of statements that may
    exist in a Bash statement tree. It is mutually recursive with 'Annotated'.
    It is a 'Foldable' and a 'Functor'.
 -}
data Statement t
  = Empty
  | SimpleCommand   (Expression t)      [Expression t]
  | NoOp            ByteString
  | Bang            (Annotated t)
  | AndAnd          (Annotated t)       (Annotated t)
  | OrOr            (Annotated t)       (Annotated t)
  | Pipe            (Annotated t)       (Annotated t)
  | Sequence        (Annotated t)       (Annotated t)
  | Background      (Annotated t)       (Annotated t)
  | Group           (Annotated t)
  | Subshell        (Annotated t)
  | Function        FuncName            (Annotated t)
  | IfThen          (Annotated t)       (Annotated t)
  | IfThenElse      (Annotated t)       (Annotated t)       (Annotated t)
  | For             Identifier          [Expression t]      (Annotated t)
  | Case            (Expression t)      [(Expression t, Annotated t)]
  | While           (Annotated t)       (Annotated t)
  | Until           (Annotated t)       (Annotated t)
--  BraceBrace      (ConditionalExpression t)
  | Assign          (Assignment t)
  | Declare         (Assignment t)
  | Local           (Assignment t)
  | Export          Identifier          (Expression t)
  | IsSet           VarName
  | ArrayUpdate     Identifier          (Expression t)      (Expression t)
  | DictUpdate      Identifier          (Expression t)      (Expression t)
  | Redirect        (Annotated t)       Redirection
                    FileDescriptor      (Either (Expression t) FileDescriptor)
  | EvalCommand     [Expression t]
deriving instance (Eq t) => Eq (Statement t)
deriving instance (Ord t) => Ord (Statement t)
deriving instance (Show t) => Show (Statement t)
instance Functor Statement where
  fmap f stmt                =  case stmt of
    Empty                   ->  Empty
    SimpleCommand cmd args  ->  SimpleCommand (f' cmd) (fmap f' args)
    NoOp b                  ->  NoOp b
    Bang ann                ->  Bang (f' ann)
    AndAnd ann ann'         ->  AndAnd (f' ann) (f' ann')
    OrOr ann ann'           ->  OrOr (f' ann) (f' ann')
    Pipe ann ann'           ->  Pipe (f' ann) (f' ann')
    Sequence ann ann'       ->  Sequence (f' ann) (f' ann')
    Background ann ann'     ->  Background (f' ann) (f' ann')
    Group ann               ->  Group (f' ann)
    Subshell ann            ->  Subshell (f' ann)
    Function fname ann      ->  Function fname (f' ann)
    IfThen ann ann'         ->  IfThen (f' ann) (f' ann')
    IfThenElse a a' a''     ->  IfThenElse (f' a) (f' a') (f' a'')
    For ident args ann      ->  For ident (fmap f' args) (f' ann)
    Case expr cases         ->  Case (f' expr) (fmap (f' *** f') cases)
    While ann ann'          ->  While (f' ann) (f' ann')
    Until ann ann'          ->  Until (f' ann) (f' ann')
--  BraceBrace      (ConditionalExpression t)
    Assign a                ->  Assign (f' a)
    Declare a               ->  Declare (f' a)
    Local a                 ->  Local (f' a)
    Export ident expr       ->  Export ident (f' expr)
    IsSet var               ->  IsSet var
    ArrayUpdate ident a b   ->  ArrayUpdate ident (f' a) (f' b)
    DictUpdate ident a b    ->  DictUpdate ident (f' a) (f' b)
    Redirect ann r fd chan  ->  Redirect (f' ann) r fd (fmapExprFD chan)
    EvalCommand es          ->  EvalCommand (fmap f' es)
   where
    f'                       =  fmap f
    fmapExprFD (Left expr)   =  Left (f' expr)
    fmapExprFD (Right fd)    =  Right fd
instance Foldable Statement where
  foldMap f stmt             =  case stmt of
    Empty                   ->  mempty
    SimpleCommand cmd args  ->  f' cmd `mappend` foldMap f' args
    NoOp _                  ->  mempty
    Bang ann                ->  f' ann
    AndAnd ann ann'         ->  f' ann `mappend` f' ann'
    OrOr ann ann'           ->  f' ann `mappend` f' ann'
    Pipe ann ann'           ->  f' ann `mappend` f' ann'
    Sequence ann ann'       ->  f' ann `mappend` f' ann'
    Background ann ann'     ->  f' ann `mappend` f' ann'
    Group ann               ->  f' ann
    Subshell ann            ->  f' ann
    Function _ ann          ->  f' ann
    IfThen ann ann'         ->  f' ann `mappend` f' ann'
    IfThenElse a a' a''     ->  foldMap f' [a, a', a'']
    For _ args ann          ->  foldMap f' args `mappend` f' ann
    Case expr cases         ->  f' expr `mappend` foldMap foldMapPair cases
    While ann ann'          ->  f' ann `mappend` f' ann'
    Until ann ann'          ->  f' ann `mappend` f' ann'
--  BraceBrace      ConditionalExpression
    Assign a                ->  f' a
    Declare a               ->  f' a
    Local a                 ->  f' a
    Export _ expr           ->  f' expr
    IsSet _                 ->  mempty
    ArrayUpdate _ a b       ->  f' a `mappend` f' b
    DictUpdate _ a b        ->  f' a `mappend` f' b
    Redirect ann _ _ chan   ->  f' ann `mappend` foldMapExprFD chan
    EvalCommand es          ->  foldMap f' es
   where
    f'                       =  foldMap f
    foldMapExprFD (Left expr) = f' expr
    foldMapExprFD (Right  _)  = mempty
    foldMapPair (x, y)       =  f' x `mappend` f' y

{-| The type of Bash expressions, handling many kinds of variable reference as
    well as eval and process substitution. It is 'Foldable' and a 'Functor'.
 -}
data Expression t            =  Literal Esc.Bash
                             |  Asterisk
                             |  QuestionMark
                             |  Tilde
                             |  ReadVar VarName
                             |  ReadVarSafe VarName
                             |  ReadArray Identifier (Expression t)
                             |  ReadArraySafe Identifier (Expression t)
                             |  ARGVElements
                             |  ARGVLength
                             |  Elements Identifier
                             |  ElementsSafe Identifier
                             |  Keys Identifier
                             |  Length VarName
                             |  Trim Trim VarName (Expression t)
                             |  ArrayLength Identifier
                             |  Concat (Expression t) (Expression t)
                             |  Eval (Annotated t)
                             |  EvalUnquoted (Annotated t)
                             |  ProcessIn (Annotated t)
                             |  ProcessOut (Annotated t)
-- TODO                      |  IndirectExpansion Identifier
-- TODO                      |  Substring, Replacement, &c.
deriving instance (Eq t) => Eq (Expression t)
deriving instance (Ord t) => Ord (Expression t)
deriving instance (Show t) => Show (Expression t)
instance IsString (Expression t) where
  fromString                 =  literal . fromString
instance Functor Expression where
  fmap f expr                =  case expr of
    Literal esc             ->  Literal esc
    Asterisk                ->  Asterisk
    QuestionMark            ->  QuestionMark
    Tilde                   ->  Tilde
    ReadVar v               ->  ReadVar v
    ReadVarSafe v           ->  ReadVarSafe v
    ReadArray ident expr    ->  ReadArray ident (fmap f expr)
    ReadArraySafe ident expr -> ReadArraySafe ident (fmap f expr)
    ARGVElements            ->  ARGVElements
    ARGVLength              ->  ARGVLength
    Elements ident          ->  Elements ident
    ElementsSafe ident      ->  Elements ident
    Keys ident              ->  Keys ident
    Length ident            ->  Length ident
    Trim trim v expr        ->  Trim trim v (fmap f expr)
    ArrayLength ident       ->  ArrayLength ident
    Concat expr expr'       ->  Concat (fmap f expr) (fmap f expr')
    Eval ann                ->  Eval (fmap f ann)
    EvalUnquoted ann        ->  EvalUnquoted (fmap f ann)
    ProcessIn ann           ->  ProcessIn (fmap f ann)
    ProcessOut ann          ->  ProcessOut (fmap f ann)
instance Foldable Expression where
  foldMap f expr             =  case expr of
    Literal _               ->  mempty
    Asterisk                ->  mempty
    QuestionMark            ->  mempty
    Tilde                   ->  mempty
    ReadVar _               ->  mempty
    ReadVarSafe _           ->  mempty
    ReadArray _ expr        ->  foldMap f expr
    ReadArraySafe _ expr    ->  foldMap f expr
    ARGVElements            ->  mempty
    ARGVLength              ->  mempty
    Elements _              ->  mempty
    ElementsSafe _          ->  mempty
    Keys _                  ->  mempty
    Length _                ->  mempty
    Trim _ _ expr           ->  foldMap f expr
    ArrayLength _           ->  mempty
    Concat expr expr'       ->  foldMap f expr `mappend` foldMap f expr'
    Eval ann                ->  foldMap f ann
    EvalUnquoted ann        ->  foldMap f ann
    ProcessIn ann           ->  foldMap f ann
    ProcessOut ann          ->  foldMap f ann

{-| Escape a 'ByteString' to produce a literal expression.
 -}
literal                     ::  ByteString -> Expression t
literal                      =  Literal . Esc.bash

data VarName                 =  VarIdent Identifier | VarSpecial SpecialVar
deriving instance Eq VarName
deriving instance Ord VarName
deriving instance Show VarName
instance IsString VarName where
  fromString                 =  fromJust . varName . fromString

varName                     ::  ByteString -> Maybe VarName
varName bytes                =  (VarSpecial <$> specialVar bytes) `mplus`
                                (VarIdent <$> identifier bytes)

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

{-| Bash functions can have surprising names. Once the word containing the
    name of the function has been identified by the Bash parser, the only
    constraint as of this writing is that it not be all digits and contain
    neither quotes nor dollar signs. Thus the following are all callable
    functions:

 >  function http://duckduckgo.com { curl -sSfL http://duckduckgo.com?q="$1" ;}
 >  function 123.0 { echo 123.0 ;}
 >  function + { echo "$@" | sed 's/ / + /g' | bc ;}

    Yet a function name may only be parsed if its surroundings constitute a
    valid function declaration. So we are not able to declare these functions:

 >  function par()ens { echo '(' "$@" ')' ;}
 >  function (parens) { echo '(' "$@" ')' ;}

    (The parser thinks the parens are there to separate the function name from
    the function body.)

    Some functions can be declared but not called. For example:

 >  function for { echo for ;}
 >  function x=y { echo x is y ;}

    Calling the former results in a syntax error. A call to the latter is
    parsed as an assignment.

    It is possible to override important builtins with function declarations.
    For example:

 >  function set { echo Haha! ;}
 >  function declare { echo Surprise! ;}

    Overall, Bash function names are quite flexible but inconsistent and
    potentially a cause of grave errors.

 -}
data FuncName                =  Simple Identifier | Fancy ByteString
deriving instance Eq FuncName
deriving instance Ord FuncName
deriving instance Show FuncName
instance IsString FuncName where
  fromString                 =  fromJust . funcName . fromString

{-| Produce a 'FuncName', choosing the 'Simple' constructor if the name is a
    simple identifier.
 -}
funcName                    ::  ByteString -> Maybe FuncName
funcName bytes               =  (Simple <$> identifier bytes)
                            <|> Fancy bytes <$ guard (not invalid)
 where
  invalid = all isDigit bytes || any (`elem` "()$'\"") bytes

{-| The names of special variables, with otherwise illegal identifiers, are
    represented by this type.
 -}
data SpecialVar
  = DollarQuestion | DollarHyphen      | DollarDollar
                   | DollarBang        | DollarUnderscore
                   | Dollar0 | Dollar1 | Dollar2 | Dollar3 | Dollar4
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
             | "$-" == b     =  Just DollarHyphen
             | "$$" == b     =  Just DollarDollar
             | "$!" == b     =  Just DollarBang
             | "$_" == b     =  Just DollarUnderscore
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
specialVarBytes DollarHyphen =  "$-"
specialVarBytes DollarDollar =  "$$"
specialVarBytes DollarBang   =  "$!"
specialVarBytes DollarUnderscore = "$_"
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


data Trim                    =  ShortestLeading  | LongestLeading
                             |  ShortestTrailing | LongestTrailing
deriving instance Eq Trim
deriving instance Ord Trim
deriving instance Show Trim

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
data ConditionalExpression t
  = File_a          (Expression t)
  | File_b          (Expression t)
  | File_c          (Expression t)
  | File_d          (Expression t)
  | File_e          (Expression t)
  | File_f          (Expression t)
  | File_g          (Expression t)
  | File_h          (Expression t)
  | File_k          (Expression t)
  | File_p          (Expression t)
  | File_r          (Expression t)
  | File_s          (Expression t)
  | File_t          (Expression t)
  | File_u          (Expression t)
  | File_w          (Expression t)
  | File_x          (Expression t)
  | File_O          (Expression t)
  | File_G          (Expression t)
  | File_L          (Expression t)
  | File_S          (Expression t)
  | File_N          (Expression t)
  | File_nt         (Expression t)      (Expression t)
  | File_ot         (Expression t)      (Expression t)
  | File_ef         (Expression t)      (Expression t)
  | OptSet          (Expression t)
  | StringEmpty     (Expression t)
  | StringNonempty  (Expression t)
  | StringEq        (Expression t)      (Expression t)
  | StringNotEq     (Expression t)      (Expression t)
  | StringLT        (Expression t)      (Expression t)
  | StringGT        (Expression t)      (Expression t)
  | StringRE        (Expression t)      (Expression t)
  | NumEq           (Expression t)      (Expression t)
  | NumNotEq        (Expression t)      (Expression t)
  | NumLT           (Expression t)      (Expression t)
  | NumLEq          (Expression t)      (Expression t)
  | NumGT           (Expression t)      (Expression t)
  | NumGEq          (Expression t)      (Expression t)
  | Not             (Expression t)      (Expression t)
  | And             (Expression t)      (Expression t)
  | Or              (Expression t)      (Expression t)
deriving instance (Eq t) => Eq (ConditionalExpression t)
deriving instance (Ord t) => Ord (ConditionalExpression t)
deriving instance (Show t) => Show (ConditionalExpression t)

data Assignment t
  = Var             Identifier          (Expression t)
  | Array           Identifier          [Expression t]
  | Dict            Identifier          [(Expression t, Expression t)]
deriving instance (Eq t) => Eq (Assignment t)
deriving instance (Ord t) => Ord (Assignment t)
deriving instance (Show t) => Show (Assignment t)
instance Functor Assignment where
  fmap f assign              =  case assign of
    Var ident expr          ->  Var ident (f' expr)
    Array ident assigns     ->  Array ident (fmap f' assigns)
    Dict ident assigns      ->  Dict ident (fmap (f' *** f') assigns)
   where
    f'                       =  fmap f
instance Foldable Assignment where
  foldMap f assign           =  case assign of
    Var _ expr              ->  f' expr
    Array _ assigns         ->  foldMap f' assigns
    Dict _ assigns          ->  foldMap foldMapPair assigns
   where
    f'                       =  foldMap f
    foldMapPair (x, y)       =  f' x `mappend` f' y


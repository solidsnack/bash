{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
  #-}
{-| Some convenient annotations for Bash scripts, provided with example
    pretty printer typeclass instances.
 -}
module Language.Bash.Annotations where

import Control.Monad
import Data.ByteString.Char8

import Language.Bash.Syntax (Statement(..))
import Language.Bash.PrettyPrinter (PP(..), Annotation(..))
import Language.Bash.PrettyPrinter.State


{-| Append some raw lines, in flow, above and below a statement.
 -}
data Lines                   =  Lines [ByteString]
                                      [ByteString]
deriving instance Eq Lines
deriving instance Ord Lines
deriving instance Show Lines
instance Annotation Lines where
  annotate (Lines above below) stmt = do unlines above
                                         pp stmt
                                         when ([] /= below)
                                              (nl >> unlines below)
   where
    unlines                  =  mapM_ (\x -> word x >> nl)


{-| Annotate a statement with statements of different types, with special
    rules for empty 'NoOp' statements -- as long as the 'ByteString'
    \"comment\" in the 'NoOp' is empty, the 'NoOp' is simply elided.
 -}
data Statements a b          =  Statements (Statement a)
                                           (Statement b)
deriving instance (Eq a, Eq b) => Eq (Statements a b)
deriving instance (Ord a, Ord b) => Ord (Statements a b)
deriving instance (Show a, Show b) => Show (Statements a b)
instance (Annotation a, Annotation b) => Annotation (Statements a b) where
  annotate (Statements above below) stmt = pp' above >> pp' stmt >> pp' below
   where
    pp' (NoOp b) | b == ""   =  return ()
    pp' x                    =  pp x


instance (PP t, PP t') => Annotation (t, t') where
  annotate (above, below) stmt = pp above >> pp stmt >> pp below


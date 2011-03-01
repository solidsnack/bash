{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
  #-}
{-| Some convenient annotations for Bash scripts, provided with example
    pretty printer typeclass instances.
 -}
module Language.Bash.Annotations where

import Data.ByteString.Char8

import Language.Bash.Syntax (Statement)
import Language.Bash.PrettyPrinter (PP(..), Annotation(..))
import Language.Bash.PrettyPrinter.State


data Lines                   =  Lines [ByteString] -- ^ Lines above.
                                      [ByteString] -- ^ Lines below.
deriving instance Eq Lines
deriving instance Ord Lines
deriving instance Show Lines

instance Annotation Lines where
  ppA (Lines above below, stmt) =
    mapM_ word above >> pp stmt >> mapM_ word below


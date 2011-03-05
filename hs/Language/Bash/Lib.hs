{-# LANGUAGE OverloadedStrings
           , NoMonomorphismRestriction
  #-}
{-| Shortcuts for Bash generation that also demonstrate use of the library.
 -}
module Language.Bash.Lib where

import Data.ByteString.Char8 (ByteString, pack)

import Language.Bash.Syntax


cmd                         ::  Expression t -> [Expression t] -> Statement t
cmd expr exprs               =  SimpleCommand expr exprs



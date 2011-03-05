{-# LANGUAGE OverloadedStrings
           , NoMonomorphismRestriction
  #-}

module Language.Bash.Util where

import Data.ByteString.Char8 (ByteString, pack)

import Language.Bash.Syntax


cmd                         ::  Expression t -> [Expression t] -> Statement t
cmd expr exprs               =  SimpleCommand expr exprs



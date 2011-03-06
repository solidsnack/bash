{-# LANGUAGE OverloadedStrings
           , NoMonomorphismRestriction
  #-}
{-| Shortcuts for Bash generation that also demonstrate use of the library.
 -}
module Language.Bash.Lib where

import Data.Monoid
import Data.ByteString.Char8 (ByteString, pack)

import Language.Bash.Syntax


cmd                         ::  Expression t -> [Expression t] -> Statement t
cmd expr exprs               =  SimpleCommand expr exprs


esed :: (Monoid m) => Identifier -> Bool -> Annotated m
esed ident d | not d         =  setGNUorBSD
             | otherwise     =  ann_ (Sequence decl setGNUorBSD)
 where
  [sed, fgrep, decl, setr, setE, checkGNU, setGNUorBSD] = fmap ann_
    [ cmd "sed" ["--version"]
    , cmd "fgrep" ["-q", "GNU"]
    , ArrayDecl ident []
    , ArrayAssign ident ["sed", "-r"]
    , ArrayAssign ident ["sed", "-E"]
    , Pipe sed fgrep
    , IfThenElse checkGNU setr setE ]


ann_                        ::  (Monoid m) => Statement m -> Annotated m
ann_                         =  Annotated mempty



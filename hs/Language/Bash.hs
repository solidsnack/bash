{-| Types and functions for generation of Bash scripts, with safe escaping
    and composition of a large subset of Bash statements and expressions.

    This module is meant to be imported qualified -- perhaps as @Bash@ -- and
    contains everything you need to build and render Bash scripts. For
    examples of usage, look at "Language.Bash.Lib".

 -}
module Language.Bash
  ( Language.Bash.Syntax.Statement(..)
  , Language.Bash.Syntax.Annotated(..)
  , Language.Bash.Syntax.Expression(..)
  , Language.Bash.Syntax.literal
  , Language.Bash.Syntax.Identifier()
  , Language.Bash.Syntax.identifier
  , Language.Bash.Syntax.SpecialVar()
  , Language.Bash.Syntax.specialVar
  , Language.Bash.Syntax.VarName(..)
  , Language.Bash.Syntax.varName
  , Language.Bash.Syntax.FuncName(..)
  , Language.Bash.Syntax.funcName
  , Language.Bash.Syntax.Redirection(..)
  , Language.Bash.Syntax.FileDescriptor(..)
  , Language.Bash.PrettyPrinter.PP(..)
  , Language.Bash.PrettyPrinter.bytes
  , Language.Bash.PrettyPrinter.builder
  , Language.Bash.PrettyPrinter.State.PPState()
  , Language.Bash.PrettyPrinter.State.render
  , Language.Bash.PrettyPrinter.State.nlCol
  , Language.Bash.Script.script
  , Language.Bash.Script.script_sha1
  , module Language.Bash.Annotations
  , module Language.Bash.Lib
  ) where

import Language.Bash.Syntax
import Language.Bash.PrettyPrinter
import Language.Bash.PrettyPrinter.State
import Language.Bash.Annotations
import Language.Bash.Script
import Language.Bash.Lib


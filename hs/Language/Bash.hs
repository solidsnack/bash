{-| Types and functions for generation of Bash scripts, with safe escaping
    and composition of a large subset of Bash statements and expressions.
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
  , Language.Bash.Syntax.Redirection(..)
  , Language.Bash.Syntax.FileDescriptor(..)
  , Language.Bash.PrettyPrinter.PP(..)
  , Language.Bash.PrettyPrinter.bytes
  , Language.Bash.PrettyPrinter.builder
  , Language.Bash.PrettyPrinter.State.PPState()
  , Language.Bash.PrettyPrinter.State.render
  , Language.Bash.PrettyPrinter.State.nlCol
--, Language.Bash.Script.script
--, Language.Bash.Script.script_sha1
  ) where

import Language.Bash.Syntax
import Language.Bash.PrettyPrinter
import Language.Bash.PrettyPrinter.State
--import Language.Bash.Script


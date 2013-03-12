{-# LANGUAGE OverloadedStrings
           , NoMonomorphismRestriction
  #-}
{-| Shortcuts for Bash generation that also demonstrate use of the library.
 -}
module Language.Bash.Lib where

import Data.Monoid
import Data.ByteString.Char8 (ByteString, pack)

import Language.Bash.Syntax


{-| Create a simple command from expressions.
 -}
cmd                         ::  Expression t -> [Expression t] -> Statement t
cmd expr exprs               =  SimpleCommand expr exprs


{-| Declare or assign an array to a @sed@ command line that will use extended
    regular expressions, checking for GNU or BSD @sed@. The 'Bool' argument
    determines whether to insert the declaration or not.
 -}
esed :: (Monoid m) => Identifier -> Annotated m
esed ident                   =  setGNUorBSD
 where
  [sed, fgrep, setr, setE, checkGNU, setGNUorBSD] = fmap ann_
    [ cmd "sed" ["--version"]
    , cmd "fgrep" ["-q", "GNU"]
    , Assign (Array ident ["sed", "-r"])
    , Assign (Array ident ["sed", "-E"])
    , dev_null (Pipe sed fgrep)
    , IfThenElse checkGNU setr setE ]


{-| Perform a statement for integer values ranging from the first integral
    parameter to the second, using @seq@.
 -}
for
 :: (Monoid m, Integral i, Show i)
 => Identifier -> i -> i -> Annotated m -> Statement m
for ident a z ann            =  For ident [EvalUnquoted (ann_ (seqAZ a z))] ann


{-| Evaluate @seq@ for the given arguments.
 -}
seqAZ                       ::  (Integral i, Show i) => i -> i -> Statement t
seqAZ a z                    =  SimpleCommand "seq" [lshow a, lshow z]
 where
  lshow                      =  literal . pack . show . toInteger


{-| A set statement that covers a few error handling options, setting
    @errexit@, @nounset@ and @pipefail@.
 -}
setSafe                     ::  Statement t
setSafe                      =  SimpleCommand "set" [ "-o", "errexit"
                                                    , "-o", "nounset"
                                                    , "-o", "pipefail" ]


{-| A statement that allows one to redirect output to a file as root. This is
    what you might expect @sudo echo x > privileged_file@ would do (though
    that does not actually work).
 -}
sudo_write                  ::  (Monoid m) => Expression m -> Statement m
sudo_write path              =  Redirect (ann_ tee) Out 1 (Left "/dev/null")
 where
  tee                        =  SimpleCommand "sudo" ["tee", path]


{-| A statement that allows one to redirect output to a file as root. This is
    what you might expect @sudo echo x > privileged_file@ would do (though
    that does not actually work).
 -}
dev_null                    ::  (Monoid m) => Statement m -> Statement m
dev_null stmt                =  Redirect (ann_ r2)   Out 1 (Left "/dev/null")
 where
  r2                         =  Redirect (ann_ stmt) Out 2 (Left "/dev/null")


{-| Annotate a statement with the 0 value of a monoid.
 -}
ann_                        ::  (Monoid m) => Statement m -> Annotated m
ann_                         =  Annotated mempty



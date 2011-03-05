{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
  #-}
{-| Utilities for turning statements into scripts and script fragments. 
 -}
module Language.Bash.Script where

import Data.Binary.Builder (Builder, fromByteString)
import Data.ByteString.Char8 (ByteString, append)
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import Data.Foldable
import Data.Monoid

import qualified Data.Digest.Pure.SHA

import Language.Bash.Syntax
import Language.Bash.Annotations
import Language.Bash.PrettyPrinter
import Language.Bash.PrettyPrinter.State


{-| Produce a script beginning with @#!\/bin\/bash@ and a safe set statement.
 -}
script                      ::  (Annotation t) => Statement t -> Builder
script statement             =  mconcat [ fromByteString "#!/bin/bash\n"
                                        , builder (setSafe :: Statement ())
                                        , fromByteString "\n\n"
                                        , builder statement ]


{-| Produce a script beginning with @#!\/bin\/bash@ and some (optional)
    documentation. Cause the script to be scanned for SHA-1 hash of the setup
    (first statement argument) and main (second statement argument) before
    running the safe set statement and running the second argument.
 -}
script_sha1
 :: forall t t'. (Annotation t, Annotation t')
 => ByteString -> Statement t -> Statement t' -> Builder
script_sha1 docs setup main  =  mconcat [ fromByteString "#!/bin/bash\n\n"
                                        , remarks
                                        , fromByteString "######## Setup."
                                        , fromByteString "\n\n"
                                        , fromByteString setup'
                                        , fromByteString "\n\n"
                                        , fromByteString "######## Main."
                                        , fromByteString "\n\n"
                                        , builder tokenCheck' ]
 where
  setup'                     =  bytes setup
  main'                      =  bytes main
  mainSafe                   =  Sequence (dance setSafe) (dance main)
  token                      =  sha1 (append setup' main')
  tokenCheck'               ::  Statement (Statements (Statements t' ()) ())
  tokenCheck'                =  tokenCheck token mainSafe
  remarks | docs == mempty   =  fromByteString ""
          | otherwise        =  fromByteString (docs `mappend` "\n\n")


{-| A set statement that covers a few error handling options, setting
    @errexit@, @nounset@ and @pipefail@.
 -}
setSafe                     ::  Statement t
setSafe                      =  SimpleCommand "set" [ "-o", "errexit"
                                                    , "-o", "nounset"
                                                    , "-o", "pipefail" ]


{-| Scan @$0@ for the token before running, producing a statement annotated
    with the initial statement. This is a bit clumsy but is used internally.
 -}
tokenCheck :: ByteString -> Statement t -> Statement (Statements t t')
tokenCheck token stmt =
  IfThen (Annotated (Statements noop noop) (tokenFGREPq token))
         (dance stmt)

{-| Scan @$0@ for the token before running, correctly producing monoidal
    annotations. The function argument provides an annotation for the @fgrep@
    check generated to search for the token. (@const mempty@ would be
    appropriate in most cases.)
 -}
mtokenCheck                 ::  (Monoid t) => ByteString
                            ->  (Statement t -> t) -> Statement t
                            ->  Statement t
mtokenCheck token f statement = IfThen (Annotated (f check) check)
                                       (Annotated (fold statement) statement)
 where
  check                      =  tokenFGREPq token

tokenFGREPq                 ::  ByteString -> Statement t
tokenFGREPq token
  = SimpleCommand "fgrep" ["-q", literal token, ReadVar (Left Dollar0)]

{-| Scan @$0@ the SHA1 of the statement before running.
 -}
sha1Check                   ::  (Annotation t, Annotation t')
                            =>  Statement t -> Statement (Statements t t')
sha1Check stmt               =  tokenCheck ((sha1 . bytes) stmt) stmt


sha1                        ::  ByteString -> ByteString
sha1                         =  Data.ByteString.Char8.pack
                             .  Data.Digest.Pure.SHA.showDigest
                             .  Data.Digest.Pure.SHA.sha1
                             .  Data.ByteString.Lazy.fromChunks
                             .  (:[])

{-| The noop dance -- annotate a 'NoOp' with a statement, essentially as a
    type coercion.
 -}
dance                       ::  Statement t -> Annotated (Statements t t')
dance stmt                   =  Annotated (Statements stmt noop) noop

noop                        ::  Statement any
noop                         =  NoOp ""


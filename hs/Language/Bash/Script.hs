{-# LANGUAGE OverloadedStrings
  #-}
{-| Utilities for turning statements into scripts and script fragments. 
 -}
module Language.Bash.Script where

import Data.Binary.Builder (Builder, fromByteString)
import Data.ByteString.Char8 (ByteString, append)
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import Data.Monoid

import qualified Data.Digest.Pure.SHA

import Language.Bash.Syntax
import Language.Bash.PrettyPrinter
import Language.Bash.PrettyPrinter.State


{-| Produce a script beginning with @#!/bin/bash@ and a safe set statement.
 -}
script                      ::  Statement -> Builder
script statement             =  mconcat [ fromByteString "#!/bin/bash\n"
                                        , builder setSafe
                                        , fromByteString "\n\n"
                                        , builder statement ]


{-| Produce a script beginning with @#!/bin/bash@ and a safe set statement.
    Cause the script to be scanned for SHA-1 hash of the setup (first argument)
    and main (second argument) before running the second argument.
 -}
script_sha1                 ::  Statement -> Statement -> Builder
script_sha1 setup main       =  mconcat [ fromByteString "#!/bin/bash\n"
                                        , builder setSafe
                                        , fromByteString "\n\n"
                                        , fromByteString "######## Setup."
                                        , fromByteString "\n\n"
                                        , fromByteString setup'
                                        , fromByteString "\n\n"
                                        , fromByteString "######## Main."
                                        , fromByteString "\n\n"
                                        , builder (tokenCheck token main) ]
 where
  setup'                     =  bytes setup
  main'                      =  bytes main
  token                      =  sha1 (append setup' main')


{-| A set statement that covers a few error handling options, setting
    @errexit@, @nounset@ and @pipefail@.
 -}
setSafe                     ::  Statement
setSafe                      =  SimpleCommand "set" [ "-o", "errexit"
                                                    , "-o", "nounset"
                                                    , "-o", "pipefail" ]


{-| Scan @$0@ for the token before running. 
 -}
tokenCheck                  ::  ByteString -> Statement -> Statement
tokenCheck token statement   =  IfThen check statement
 where
  check = SimpleCommand "fgrep" ["-q", literal token, ReadVar (Left Dollar0)]


{-| Scan @$0@ the SHA1 of the statement before running.
 -}
sha1Check                   ::  Statement -> Statement
sha1Check stmt               =  tokenCheck ((sha1 . bytes) stmt) stmt


sha1                        ::  ByteString -> ByteString
sha1                         =  Data.ByteString.Char8.pack
                             .  Data.Digest.Pure.SHA.showDigest
                             .  Data.Digest.Pure.SHA.sha1
                             .  Data.ByteString.Lazy.fromChunks
                             .  (:[])


{-# LANGUAGE OverloadedStrings
  #-}
module Language.Bash.Script where

import Data.Binary.Builder (Builder, fromByteString)
import Data.Monoid

import Language.Bash.Syntax
import Language.Bash.PrettyPrinter
import Language.Bash.PrettyPrinter.State


{-| Produce a script beginning with @#!/bin/bash@ and a safe set statement. 
 -}
script                      ::  Statement -> Builder
script statement             =  mconcat [ fromByteString "#!/bin/bash\n"
                                        , (render (nlCol 0) . pp) setSafe
                                        , fromByteString "\n\n"
                                        , (render (nlCol 0) . pp) statement ]

{-| A set statement that covers a few error handling options, setting
    @errexit@, @nounset@ and @pipefail@. 
 -}
setSafe                     ::  Statement
setSafe                      =  SimpleCommand "set" [ "-o", "errexit"
                                                    , "-o", "nounset"
                                                    , "-o", "pipefail" ]



{-# LANGUAGE ForeignFunctionInterface #-}
module Foo where

import Foreign.C


foreign export ccall foo    ::  CInt

foo                         ::  CInt
foo                          =  42


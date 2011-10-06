{-# LANGUAGE EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
           , GeneralizedNewtypeDeriving
           , NoMonomorphismRestriction
  #-}
{-| A monadic interface for constructing Bash statements.
 -}
module Language.Bash.Monad where

import Language.Bash.Syntax


type Monad t                 =  Monad { combine    :: t -> t -> t,
                                        measure    :: Statement t -> t,
                                        tree       :: Annotated t       }

instance Monad Annotated where
  return                     =  Annotated t (NoOp "Monad.return")
  (Annotated t stmt) >>= k   =  do
    (combine, measure)      <-  ask
    Annotated (combine t t') (Sequence stmt stmt')
   where
    Annotated t' stmt'       =  k t

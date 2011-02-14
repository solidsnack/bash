{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , NamedFieldPuns
           , NoMonomorphismRestriction
  #-}
{-| Pretty printer for Bash. The pretty printer generates a builder which we
    pass to the "linker" later to put between the two main chunks of code.
 -}
module Language.Bash.PrettyPrinter.State where

import qualified Data.List as List
import Data.Monoid
import Prelude hiding (concat, length, replicate)
import Data.Binary.Builder (Builder, toLazyByteString)
import qualified Data.Binary.Builder as Builder
import Data.ByteString.Char8
import Data.ByteString.Lazy (toChunks)
import Data.Word
import Control.Monad.State.Strict


{-| State of pretty printing -- string being built, indent levels, whether
    we've started a new line or not. 
 -}
data PPState                 =  PPState { indents :: [Word]
                                        , flag :: Bool
                                        , columns :: Word
                                        , string :: Builder }

render                      ::  PPState -> State PPState () -> Builder
render init computation      =  string $ execState computation init

renderBytes                 ::  PPState -> State PPState () -> ByteString
renderBytes = ((concat . toChunks . toLazyByteString) .) . render

{-| Pretty printer state starting on a new line indented to the given column.
 -}
nlCol                       ::  Word -> PPState
nlCol w                      =  PPState [w] True 0 Builder.empty


{-| Operations we can perform while pretty printing:

 *  Add n spaces to the indentation.

 *  Strip off a level of indentation.

 *  Move to the next line.

 *  Append a shell word to the current line.

 -}
data PPOp = Indent Word | Outdent | Word ByteString | Newline


{-| Apply an operation to a state. 
 -}
op                          ::  PPState -> PPOp -> PPState
op state@PPState{..} x       =  case x of
  Indent w                  ->  state {indents = w:indents}
  Outdent                   ->  state {indents = nullTail indents}
  Newline                   ->  state {string = sNL, flag = True, columns = 0}
  Word b                    ->  state {string = s', flag = False, columns = c'}
   where
    c'                       =  columns + cast (length padded)
    s'                       =  string `mappend` Builder.fromByteString padded
    dent                     =  cast (sum indents)
    padded                   =  if flag then replicate dent ' ' `append` b
                                        else ' ' `cons` b
 where
  nullTail list              =  if list == [] then list else List.tail list
  sNL                        =  string `mappend` Builder.fromByteString "\n"

opM                         ::  [PPOp] -> State PPState ()
opM                          =  mapM_ (modify . flip op)

nl                          ::  State PPState ()
nl                           =  opM [Newline]
hang                        ::  ByteString -> State PPState ()
hang b                       =  opM [Word b, Indent (cast (length b) + 1)]
word                        ::  ByteString -> State PPState ()
word b                       =  opM [Word b]
wordcat                     ::  [ByteString] -> State PPState ()
wordcat                      =  word . concat
outdent                     ::  State PPState ()
outdent                      =  opM [Outdent]
inword                      ::  ByteString -> State PPState ()
inword b                     =  opM [Word b, Indent 2, Newline]
outword                     ::  ByteString -> State PPState ()
outword b                    =  opM [Newline, Outdent, Word b]

breakline                   ::  ByteString -> State PPState ()
breakline b                  =  do
  PPState{..}               <-  get
  if columns + cast (length b) + 1 > 79 && columns /= sum indents
    then  opM [Word "\\", Newline, Word b]
    else  opM [Word b]

cast                         =  fromIntegral


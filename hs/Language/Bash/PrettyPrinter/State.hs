{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , NamedFieldPuns
           , NoMonomorphismRestriction
  #-}
{-| Pretty printer state, used within a state monad computation.
 -}
module Language.Bash.PrettyPrinter.State where

import qualified Data.List as List
import Data.Monoid
import Prelude hiding (lines, round, concat, length, replicate)
import Data.Binary.Builder (Builder, toLazyByteString)
import qualified Data.Binary.Builder as Builder
import Data.ByteString.Char8 hiding (null)
import Data.ByteString.Lazy (toChunks)
import Data.Word
import Control.Monad.State.Strict


{-| State of pretty printing -- string being built, indent levels, present
    column, brace nesting.
 -}
data PPState                 =  PPState { indents :: [Word]
                                        , curly :: [()]
                                        , round :: [()]
                                        , columns :: Word
                                        , string :: Builder }
instance Show PPState where
  show PPState{..}           =  "PPState { indents=" ++ show indents
                                      ++ " curly="   ++ show curly
                                      ++ " round="   ++ show round
                                      ++ " columns=" ++ show columns
                                      ++ " string="  ++ "..." ++ " }"

{-| Produce a builder from a pretty printer state computation.
 -}
render                      ::  PPState -> State PPState () -> Builder
render init computation      =  string $ execState computation init

renderBytes                 ::  PPState -> State PPState () -> ByteString
renderBytes = ((concat . toChunks . toLazyByteString) .) . render

{-| Pretty printer state starting on a new line indented to the given column.
 -}
nlCol                       ::  Word -> PPState
nlCol w                      =  PPState [w] [()] [()] 0 Builder.empty


{-| Operations we can perform while pretty printing.
 -}
data PPOp                    =  Indent Word -- ^ Indent by N spaces.
                             |  Outdent -- ^ Remove and indentation level.
                             |  Word ByteString -- ^ Add a word to a line.
                             |  Newline -- ^ Move to newline.
                             |  Curly Bool -- ^ Introduce a level of braces.
                             |  Round Bool -- ^ Introduce a level of parens.


{-| Apply an operation to a state.
 -}
op                          ::  PPState -> PPOp -> PPState
op state@PPState{..} x       =  case x of
  Indent w                  ->  state { indents = w:indents }
  Outdent                   ->  state { indents = tSafe indents }
  Curly f | f               ->  state { indents = 2:indents, curly = ():curly
                                      , string = curly_s, columns = columns+2 }
          | otherwise       ->  state { indents = tSafe indents
                                      , curly = tSafe curly, string = s_curly }
  Round f | f               ->  state { indents = 2:indents, round = ():round
                                      , string = round_s, columns = columns+2 }
          | otherwise       ->  state { indents = tSafe indents
                                      , round = tSafe round, string = s_round }
  Newline | columns == 0    ->  state
          | otherwise       ->  state { string = sNL, columns = 0 }
  Word b                    ->  state { string = s', columns = c' }
   where
    c'                       =  columns + cast (length padded)
    s'                       =  string `mappend` Builder.fromByteString padded
    dent                     =  cast (sum indents)
    padded | columns == 0    =  replicate dent ' ' `append` b
           | otherwise       =  ' ' `cons` b
 where
  tSafe list                 =  if null list then [] else List.tail list
  sNL                        =  string `mappend` Builder.fromByteString "\n"
  curly_s                    =  Builder.fromByteString "{" `mappend` string
  s_curly                    =  string `mappend` Builder.fromByteString " ;}"
  round_s                    =  Builder.fromByteString "(" `mappend` string
  s_round                    =  string `mappend` Builder.fromByteString " )"

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
curlyOpen                   ::  State PPState ()
curlyOpen                    =  opM [Curly True]
curlyClose                  ::  State PPState ()
curlyClose                   =  opM [Curly False]
roundOpen                   ::  State PPState ()
roundOpen                    =  opM [Round True]
roundClose                  ::  State PPState ()
roundClose                   =  opM [Round False]
indentPadToNextWord         ::  State PPState ()
indentPadToNextWord          =  do
  PPState{..}               <-  get
  let x                      =  sum indents
      columns'               =  columns + 1
      indent | columns' > x  =  columns' - x
             | otherwise     =  0
  opM [Indent indent]

cast                         =  fromIntegral


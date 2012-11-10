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
                                        , separated :: Bool
                                        , string :: Builder }
instance Show PPState where
  show PPState{..}           =  "PPState { indents="   ++ show indents
                                      ++ " curly="     ++ show curly
                                      ++ " round="     ++ show round
                                      ++ " columns="   ++ show columns
                                      ++ " separated=" ++ show separated
                                      ++ " string="    ++ "..." ++ " }"

{-| Produce a builder from a pretty printer state computation.
 -}
render                      ::  PPState -> State PPState () -> Builder
render init computation      =  string $ execState computation init

renderBytes                 ::  PPState -> State PPState () -> ByteString
renderBytes = ((concat . toChunks . toLazyByteString) .) . render

{-| Pretty printer state starting on a new line indented to the given column.
 -}
nlCol                       ::  Word -> PPState
nlCol w                      =  PPState [w] [()] [()] 0 True Builder.empty


{-| Operations we can perform while pretty printing.
 -}
data PPOp                    =  Indent Word -- ^ Indent by N spaces.
                             |  Outdent -- ^ Remove an indentation level.
                             |  Bytes ByteString -- ^ Add bytes to the script.
                             |  Newline -- ^ Move to newline.
                             |  WordSeparator -- ^ Separate words with space.
                             |  Curly Bool -- ^ Introduce a level of braces.
                             |  Round Bool -- ^ Introduce a level of parens.


{-| Apply an operation to a state.
 -}
op                          ::  PPState -> PPOp -> PPState
op state@PPState{..} x       =  case x of
  Indent w                  ->  state { indents = w:indents }
  Outdent                   ->  state { indents = tSafe indents }
  Curly f | f               ->  state { indents = 2:indents, curly = ():curly
                                      , string = curly_s, columns = columns+2
                                      , separated = True }
          | otherwise       ->  state { indents = tSafe indents
                                      , curly = tSafe curly, string = s_curly
                                      , separated = False }
  Round f | f               ->  state { indents = 2:indents, round = ():round
                                      , string = round_s, columns = columns+2
                                      , separated = True }
          | otherwise       ->  state { indents = tSafe indents
                                      , round = tSafe round, string = s_round
                                      , separated = False }
  WordSeparator             ->  state { separated = False }
  Newline | columns == 0    ->  state { separated = True }
          | otherwise       ->  state { string = sNL, columns = 0
                                      , separated = True          }
  Bytes b                   ->  state { string = s', columns = c'
                                      , separated = True          }
   where
    c'                       =  columns + cast (length padded + length sSep)
    s'                       =  sappend padded
    padded                   =  mappend dent b
 where
  dent | columns == 0        =  cast (sum indents) `replicate` ' '
       | otherwise           =  ""
  sappend = mappend string . Builder.fromByteString . mappend sSep
  tSafe list                 =  if null list then [] else List.tail list
  sNL                        =  mappend string (Builder.fromByteString "\n")
  curly_s                    =  sappend (mappend dent "{")
  s_curly                    =  sappend ";}"
  round_s                    =  sappend (mappend dent "(")
  s_round                    =  sappend ")"
  sSep | not separated       =  " "
       | otherwise           =  ""

opM                         ::  [PPOp] -> State PPState ()
opM                          =  mapM_ (modify . flip op)

nl                          ::  State PPState ()
nl                           =  opM [Newline]
hang                        ::  ByteString -> State PPState ()
hang b                       =  opM [Bytes b, Indent (cast (length b))]
hangWord                    ::  ByteString -> State PPState ()
hangWord b = opM [Bytes b, Indent (cast (length b) + 1), WordSeparator]
word                        ::  ByteString -> State PPState ()
word b                       =  opM [Bytes b, WordSeparator]
wordcat                     ::  [ByteString] -> State PPState ()
wordcat                      =  word . concat
outdent                     ::  State PPState ()
outdent                      =  opM [Outdent]
inword                      ::  ByteString -> State PPState ()
inword b                     =  opM [Bytes b, Indent 2, Newline]
outword                     ::  ByteString -> State PPState ()
outword b                    =  opM [Newline, Outdent, Bytes b, WordSeparator]
curlyOpen                   ::  State PPState ()
curlyOpen                    =  opM [Curly True, WordSeparator]
curlyClose                  ::  State PPState ()
curlyClose                   =  opM [Curly False, WordSeparator]
roundOpen                   ::  State PPState ()
roundOpen                    =  opM [Round True, WordSeparator]
roundClose                  ::  State PPState ()
roundClose                   =  opM [Round False, WordSeparator]

{-| This procedure is used in printing statements within evals, to set up
    indentation correctly for lines /following/ the first line. It ensures
    that the second and following lines are printed aligned with the first
    character of the first line of the statement, not the first character of
    the @$(@, @>(@ or @<(@ enclosing the eval.
 -}
indentPadToNextWord         ::  State PPState ()
indentPadToNextWord          =  do
  PPState{..}               <-  get
  let i                      =  sum indents
      columns' | separated   =  columns
               | otherwise   =  columns + 1
      indent | columns' > i  =  columns' - i
             | otherwise     =  0
  opM [Indent indent]

cast                         =  fromIntegral

-- Debug renderer.
renderIndents indents        =  (mconcat . Prelude.reverse)
                                (Prelude.map prettify_indent indents)
 where
  prettify_indent 0          =  ""
  prettify_indent 1          =  "|"
  prettify_indent 2          =  "-|"
  prettify_indent n          =  "-" `mappend` prettify_indent (n-1)


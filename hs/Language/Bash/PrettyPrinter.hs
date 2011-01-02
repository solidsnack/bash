{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , RecordWildCards
           , NamedFieldPuns
           , NoMonomorphismRestriction
  #-}

{-| Pretty printer for Bash. The pretty printer generates a builder which we
    pass to the "linker" later to put between the two main chunks of code.
 -}

module Language.TaskL.Bash.PrettyPrinter where

import qualified Data.List as List
import Data.Monoid
import Prelude hiding (concat, length, replicate)
import Data.Binary.Builder (Builder)
import qualified Data.Binary.Builder as Builder
import Data.ByteString.Char8
import Data.Word
import Control.Monad.State.Strict

import Language.TaskL.Bash.Program


builder                     ::  PPState -> Term -> Builder
builder init t               =  string $ execState (ops t) init


ops                         ::  Term -> State PPState ()
ops term                     =  case term of
  SimpleCommand cmd vals    ->  hang cmd >> mapM_ breakline vals >> outdent
  Empty                     ->  word ": 'Do nothing.'"
  Bang t                    ->  hang "!" >> ops t >> outdent
  And t t'                  ->  ops t >> word "&&" >> nl >> ops t'
  Or t t'                   ->  ops t >> word "||" >> nl >> ops t'
  Pipe t t'                 ->  ops t >> word "|"  >> nl >> ops t'
  Sequence t t'             ->  ops t              >> nl >> ops t'
  Background t t'           ->  ops t >> word "&"  >> nl >> ops t'
  Group t                   ->  hang "{"  >> ops t >> word ";}" >> outdent
  Subshell t                ->  hang "("  >> ops t >> word ")"  >> outdent
  Function b t              ->  wordcat ["function ", b]
                            >>  inword " {" >> ops t >> outword "}"
  IfThen t t'               ->  hang "if" >> ops t >> outdent >> nl
                            >>  inword "then"      >> ops t'  >> outword "fi"
  IfThenElse t t' t''       ->  hang "if" >> ops t >> outdent >> nl
                            >>  inword "then"      >> ops t'  >> outdent
                            >>  inword "else"      >> ops t'' >> outword "fi"
  ForDoDone var vals t      ->  hang (concat ["for ", var, " in"])
                            >>  mapM_ breakline vals
                            >>  outdent >> nl
                            >>  inword "do" >> ops t >> outword "done"
  VarAssign var val         ->  wordcat [var, "=", val]
  DictDecl var pairs        ->  wordcat ["declare -A ", var, "=("] >> nl
                            >>  mapM_ (opM .  arrayset) pairs
                            >>  nl >> word ")"
  DictUpdate var key val    ->  wordcat [var, "[", key, "]=", val]
  DictAssign var pairs      ->  wordcat [var, "=("] >> nl
                            >>  mapM_ (opM .  arrayset) pairs >> word ")"
 where
  nl                         =  opM [Newline]
  hang b                     =  opM [Word b, Indent (cast (length b) + 1)]
  word b                     =  opM [Word b]
  wordcat                    =  word . concat
  outdent                    =  opM [Outdent]
  inword b                   =  opM [Word b, Indent 2, Newline]
  outword b                  =  opM [Newline, Outdent, Word b]
  arrayset (key, val)        =  [Word (concat ["[", key, "]=", val]), Newline]
  breakline b                =  do
    PPState{..}             <-  get
    if columns + cast (length b) + 1 > 79 && columns /= sum indents
      then  opM [Word "\\", Newline, Word b]
      else  opM [Word b]

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  Line break algorithm for simple commands and arguments of for loops:

    IF the width of the current line with the given word added is greater
       than 79 columns
    THEN
      IF moving the word to the following line causes the line to be shorter
      THEN
        do it
      ELSE
        add the current word to the current line
      DONE
    ELSE
      add the current word to the current line
    DONE

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}


{-| State of pretty printing -- string being built, indent levels, whether
    we've started a new line or not. 
 -}
data PPState                 =  PPState { indents :: [Word]
                                        , flag :: Bool
                                        , columns :: Word
                                        , string :: Builder }

{-| Pretty printer state starting on a new line indented to the given column.
 -}
colPPState                  ::  Word -> PPState
colPPState w                 =  PPState [w] True 0 Builder.empty


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


cast                         =  fromIntegral


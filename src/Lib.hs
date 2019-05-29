module Lib where

import           Data.Char
import           Control.Monad

-- | The 'Op' ADT represents all the instructions defined in the Brainfuck spec.
data Op = SpInc     -- ^ stack pointer increment, aka move right. Symbol: >
        | SpDec     -- ^ stack pointer decrement, aka move left. Symbol: <
        | CellInc   -- ^ increment current value. Symbol: +
        | CellDec   -- ^ decrement current value. Symbol: -
        | Out       -- ^ print current value to stdout. Symbol: .
        | In        -- ^ read a character from stdin, cast it into a number and
                    --   store it as the current value. Symbol: ,
        | Loop [Op] -- ^ execute a list of operation until the current value is 0.
                    --   Pattern: [OP...]
    deriving (Show)


-- |Give a "pretty" representation of an 'Op' that is in the same format as the
-- spec. A couple of examples 
--
-- >>> prettyOp CellInc
-- "+"
--
-- >>> prettyOp (Loop [Out, In, SpInc, Loop []])
-- "[.,>[]]"
--
-- Hint: the idea here is to pattern match against the 'Op' and return the
-- proper symbol. To implement the function for 'Loop' you might want to take a
-- look at 'map' and 'concat' (or 'concatMap' if you're feeling brave) ;).
prettyOp :: Op -> String
prettyOp CellInc     = "+"
prettyOp CellDec     = "-"
prettyOp SpInc       = ">"
prettyOp SpDec       = "<"
prettyOp Out         = "."
prettyOp In          = ","
prettyOp (Loop loop) = '[' : concatMap prettyOp loop ++ "]"


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


------------------------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------------------------

-- | 'parse' a Brainfuck program into a list of 'Op'. In case there are any
-- unsupported symbols the parser is totally free to error out ;).
--
-- Here are some examples:
--
-- >>> parse "+-.,><[><][[]]."
-- [CellInc, CellDec, Out, In, SpInc, SpDec, Loop [SpInc, SpDec], Loop [[]], Out]
--
parse :: String -> [Op]
parse "" = []
parse s  = let (op, rest) = parseOp s in op : parse rest

-- | 'parseOp' parses the next 'Op' in a given 'String'. However, since Haskell
-- is immutable by default we also need a way to "advance" over the input. The
-- easiest way to do so is probably by returning the remaining input along with
-- the parsed 'Op'.
--
-- For most of the instructions the implementation is pretty much
-- straightforward, but for loops things get tricky.
--
-- However, the general idea is to pattern match over the first character of
-- the input and when it's not a "[" then simply return a tuple made of the
-- 'Op' and the rest of the string. On the other hand, when it's a "]" just
-- delegate the actual to the `parseLoopBody` utility function that you still
-- have to implement :).
--
-- Here are some examples:
--
-- >>> parseOp "+"
-- (CellInc, "")
--
-- >>> parseOp ".>>>>>>"
-- (Out, ">>>>>>")
--
-- >>> parseOp "[]"
-- (Loop [], "")
--
-- >>> parseOp "[+-[]],"
-- (Loop [CellInc, CellDec, Loop []], ",")
--
parseOp :: String -> (Op, String)
parseOp ('>' : rest) = (SpInc, rest)
parseOp ('<' : rest) = (SpDec, rest)
parseOp ('+' : rest) = (CellInc, rest)
parseOp ('-' : rest) = (CellDec, rest)
parseOp ('.' : rest) = (Out, rest)
parseOp (',' : rest) = (In, rest)
parseOp ('[' : rest) =
  let (loop, rest') = parseLoopBody rest in (Loop loop, rest')


-- | 'parseLoopBody' is meant to parse all the 'Op' inside a loop and stop when
-- it finds the end of the loop.
--
-- The idea here is similar to 'parseOp':
--
-- * when the input starts with a "]" then the loop stops we're done and all we
--   need to do is stop parsing the body. Pay attention that the rest of the
--   string starts /after/ the "]".
--
-- * otherwise we have to parse the next op with 'parseOp', recursively call
--   'parseLoopBody' to parse the rest of the loop and eventually combine the
--   results.
--
parseLoopBody :: String -> ([Op], String)
parseLoopBody (']' : rest) = ([], rest)
parseLoopBody s =
  let (atom , s'  ) = parseOp s
      (atoms, rest) = parseLoopBody s'
  in  (atom : atoms, rest)

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


------------------------------------------------------------------------------------------
-- Interpreter
------------------------------------------------------------------------------------------

-- | 'Tape' is an /infinite/ stream of cells that contains the state of a
-- Brainfuck program.
--
-- This stream is actually made of three components: left, current and right
-- where:
--   * right contains all the cells to the right of current
--   * current is the value of the current cell
--   * left contains all the cells to the left of current in /reverse order/
--     (that is the closest to current the closest to the start of the list)
--
data Tape =
  Tape
    { left :: [Int]
    , current :: Int
    , right :: [Int]
    }
  deriving (Show)

-- | 'zeroedTape' is the starting state which is just an infinite series of 0. 
zeroedTape :: Tape
zeroedTape = Tape {left = repeat 0, current = 0, right = repeat 0}


-- | 'run' all the 'Op's over the the initial 'Tape' returning the final one. 
--
-- There are a couple of ways to implement this in terms of 'runOp':
--
--   * plain recursion: manually pattern match over the list, call `runOp` on
--     the first instruction and then call `run` again with the rest of the list
--     and the new 'Tape'.
--   * you might realize that 'run' is actually nothing more than a `foldl`!
--
-- Here are some examples
--
-- >>> current (run zeroedTape (parse "++<-->+-+"))
-- 3
--
-- >>> current (run zeroedTape (parse "++>+++++[<+>-]<"))
-- 7
--
run :: Tape -> [Op] -> Tape
run tape ops = error "TODO"

-- | 'runOp' runs only instructions that are pure (for now) over the given tape
-- which are: 'SpInc', 'SpDec', 'CellInc', 'CellDec' and 'Loop'.
--
-- 'CellInc' and 'CellDec' should be quite easy to implement because they're
-- just a matter of updating the 'current' field.
--
-- 'SpInc' and 'SpDec' are a bit more tricky. For example, 'SpInc' should move
-- 'current' to 'left' and the head of 'right' into 'current' (removing it from
-- 'right'). 'SpDec' is symmetric to 'SpInc'. Pattern matching always works,
-- but you might find the functions 'head' and 'tail` more convinient in this
-- case ;).
--
-- Lastly, 'Loop' might sound intimidating, but we've already implemented most
-- of it. If 'current' is not 0 then it 'run' its body and recurses, otherwise
-- it returns the state unchanged.
--
-- For any doubts about the syntax please check out the appropriate section in
-- Cheatsheet.
--
runOp :: Tape -> Op -> Tape
runOp _    In  = error "In is not pure, cannot implement"
runOp _    Out = error "Out is not pure, cannot implement"
runOp tape op  = error "TODO"


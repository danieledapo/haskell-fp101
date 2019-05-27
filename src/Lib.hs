module Lib where

-- Step 0: general introduction (syntax, strings, numbers, lists, functions, pattern matching)
-- Step 1: define Op (introduce ADT)
-- Step 2: pretty print Op
-- Step 3: parse Op (introduce functions and immutability)
-- Step 4: define Tape (explain laziness and the trick that splits the vector in left, curr, right)
-- Step 5: implement a pure interpreter ignoring In and Out (nothing to introduce here, I think)
-- Step 6: implement an impure interpreter over IO

import           Data.Char
import           Control.Monad

------------------------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------------------------
data Op = SpInc | SpDec | CellInc | CellDec | Out | In | Loop [Op] deriving (Show)

prettyOp :: Op -> String
prettyOp CellInc     = "+"
prettyOp CellDec     = "-"
prettyOp SpInc       = ">"
prettyOp SpDec       = "<"
prettyOp Out         = "."
prettyOp In          = ","
prettyOp (Loop loop) = '[' : concatMap prettyOp loop ++ "]"

parse :: String -> [Op]
parse "" = []
parse s  = let (atom, rest) = parseAtom s in atom : parse rest

parseAtom :: String -> (Op, String)
parseAtom ('>' : rest) = (SpInc, rest)
parseAtom ('<' : rest) = (SpDec, rest)
parseAtom ('+' : rest) = (CellInc, rest)
parseAtom ('-' : rest) = (CellDec, rest)
parseAtom ('.' : rest) = (Out, rest)
parseAtom (',' : rest) = (In, rest)
parseAtom ('[' : rest) =
  let (loop, rest') = parseLoopAtom rest in (Loop loop, rest')
 where
  parseLoopAtom (']' : rest) = ([], rest)
  parseLoopAtom s =
    let (atom , s'  ) = parseAtom s
        (atoms, rest) = parseLoopAtom s'
    in  (atom : atoms, rest)

------------------------------------------------------------------------------------------
-- Tape
------------------------------------------------------------------------------------------

data Tape = Tape { leftStack :: [Int], curr :: Int, rightStack :: [Int] } deriving (Show)

newTape :: Tape
newTape = Tape {leftStack = repeat 0, curr = 0, rightStack = repeat 0}

------------------------------------------------------------------------------------------
-- Pure Interpreter
------------------------------------------------------------------------------------------
-- runAll :: Tape -> [Op] -> Tape
-- runAll = foldl run
-- 
-- run :: Tape -> Op -> Tape
-- run m       CellInc = m { curr = curr m + 1 }
-- run m       CellDec = m { curr = curr m - 1 }
-- run tape SpInc   = Tape
--   { leftStack  = curr tape : leftStack tape
--   , curr       = head (rightStack tape)
--   , rightStack = tail (rightStack tape)
--   }
-- run tape SpDec = Tape
--   { leftStack  = tail (leftStack tape)
--   , curr       = head (leftStack tape)
--   , rightStack = curr tape : rightStack tape
--   }
-- run tape (Loop loop)
--   | curr tape /= 0 = run (foldl run tape loop) (Loop loop)
--   | otherwise         = tape

------------------------------------------------------------------------------------------
-- ImPure Interpreter
------------------------------------------------------------------------------------------
runAll :: Tape -> [Op] -> IO Tape
runAll = foldM run

run :: Tape -> Op -> IO Tape
run m    CellInc = pure $ m { curr = curr m + 1 }
run m    CellDec = pure $ m { curr = curr m - 1 }
run tape Out     = do
  putChar . chr . curr $ tape
  return tape
run tape In = do
  line <- getLine
  pure $ tape { curr = ord . head $ line }
run tape SpInc = pure $ Tape
  { leftStack  = curr tape : leftStack tape
  , curr       = head (rightStack tape)
  , rightStack = tail (rightStack tape)
  }
run tape SpDec = pure $ Tape
  { leftStack  = tail (leftStack tape)
  , curr       = head (leftStack tape)
  , rightStack = curr tape : rightStack tape
  }
run tape (Loop loop)
  | curr tape == 0 = pure tape
  | otherwise = do
    tape' <- foldM run tape loop
    run tape' (Loop loop)


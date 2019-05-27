-- Step 0: general introduction (syntax, strings, numbers, lists, functions, pattern matching)
-- Step 1: define Op (introduce ADT)
-- Step 2: parse Op (introduce functions and immutability)
-- Step 3: pretty print Op to sanity check input (introduce typeclass)
-- Step 4: define Tape (explain laziness and the trick that splits the vector in left, curr, right)
-- Step 5: implement a pure interpreter ignoring In and Out (nothing to introduce here, I think)
-- Step 6: implement an impure interpreter over IO

import           Control.Monad

import           Lib

------------------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------------------
main :: IO ()
main = do
  contents <- getContents
  let filtered = filter (`elem` "[].,<>+-") contents
  let parsed   = parse filtered

  -- sanity check
  print (concatMap prettyOp parsed)
  print filtered
  print (filtered == concatMap prettyOp parsed)

  -- print . runAll newTape $ parsed
  void . runAll newTape $ parsed

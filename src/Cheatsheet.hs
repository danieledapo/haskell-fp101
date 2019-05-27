module Cheatsheet where

import           Control.Monad.Fix


----------------------------------------------------
-- Mantra
----------------------------------------------------
-- when in doubt check the solution (:troll:)
solution :: String
solution = fix error


----------------------------------------------------
-- Variables
----------------------------------------------------
-- variables are immutable, in fact in Haskell is probably more common to refer
-- to them as bindings because they're more of a placeholder to type less than
-- something that can change.
aNumber :: Int
aNumber = 1

aDouble :: Double
aDouble = 42.0

aString :: String
aString = "Fly, you fools!"


----------------------------------------------------
-- Functions
----------------------------------------------------

-- functions types are denoted by types separated by the `->` symbol. The last
-- type is the return type and all the other ones are the arguments to the
-- function. 
-- A function body is _always_ a single expression.
plus :: Int -> Int -> Int
plus a b = a + b

-- Note that no parentheses are required around function arguments nor for
-- calling a function. This can cause some confusion if you're not used to this
-- syntax, feel free to surround function calls in parentheses when in doubt ;).
plusOne :: Int -> Int
plusOne a = plus a 1

-- binary functions can be made infix by surrounding them with `\`\`` which is
-- sometimes handy.
isOdd :: Int -> Bool
isOdd n = n `mod` 2 == 1

-- everything in haskell is an expression and `if` is no difference. It always
-- has an `else` and both of its branches must return the same type.
makeEven :: Int -> Int
makeEven n = if isOdd n then n + 1 else n

-- Haskell supports lambda functions which ~almost~ work the exact same way as
-- plain regular functions. The syntax is `\<arg1> <arg2> <arg3>... -> <body>`.
-- If you squint hard enough you might see `\` as the closest character to the
-- greek lambda character.
aLambdaIsAllYouNeed :: Int -> Int -> Int
aLambdaIsAllYouNeed = \a b -> a - b

-- functions can patterm match on its arguments to specify different
-- implementations for different inputs which might look weird at first, but it
-- really improves readability imho. Also, math code reads better :).
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- it's possible to have local bindings inside a function by using the `let in`
-- syntax. Note that the values are still immutable and again you can think of
-- them as placeholders.
veryComplexMathFunction :: Double -> Double -> Double -> (Double, Double)
veryComplexMathFunction a b c =
  let d  = sqrt (b ** 2 - 4 * a * c)
      a2 = a * 2
  in  ((-b - d) / a2, (-b + d) / a2)


----------------------------------------------------
-- Lists (and String)
----------------------------------------------------
-- Note that `String` is just an alias for `[Char]` so all functions that work
-- on lists also work on `String`.

aList :: [Int]
aList = [1, 2, 3, 4, 5]

anInfiniteList :: [Int]
anInfiniteList = [1 ..]

-- `:`prepends the left element to the right list. O(1).
singletonList :: a -> [a]
singletonList x = x : []

-- `++` merges two lists together
listConcat :: [a] -> [a] -> [a]
listConcat a b = a ++ b

-- haskell does not have for loops, use recursion instead ;). It's possible to
-- use pattern match on a list. We can pattern match on the empty list `[]` or
-- on a non empty list by breaking it down into an `(head : rest)`
-- pattern(parentheses are required in this case).
listLength :: [a] -> Int
listLength []       = 0
listLength (x : xs) = 1 + listLength xs

-- useful list functions:
-- - `head  :: [a] -> a` returns the first element of a non empty list
-- - `tail  :: [a] -> [a]` returns the tail of the list
-- - `map   :: (a -> b) -> [a] -> [b]` transforms a list of `a` to a list of
--   `b` calling a given conversion function on each element
--   `concat :: [[a]] -> [a]` -> to flatten a list of lists into a plain list
-- - `foldl :: (acc -> a -> acc) -> acc -> [b] -> b` to reduce a list of `b`
--   into a single value of type `acc` by using a "combining function"

prettyPrintList :: [Int] -> String
prettyPrintList ns = '[' : concat (map show ns) ++ "]"

listLengthFold :: [a] -> Int
listLengthFold xs = foldl (\len _ -> len + 1) 0 xs


----------------------------------------------------
-- Data Types
----------------------------------------------------
-- Point here is analogous to a struct with two `Int`s that can be referenced
-- only by pattern matching.
data Point = Point Int Int deriving (Show)

originPoint :: Point
originPoint = Point 0 0

prettyPrintPoint :: Point -> String
prettyPrintPoint (Point x y) = "P(" ++ show x ++ ", " ++ show y ++ ")"

-- A struct can have named fields in which case the names of the fields are
-- functions that can be used as getters.
data Movie = Movie
  { movieTitle :: String
  , movieDirector :: String
  , movieCast :: [String]
  , movieBudget :: Double
  } deriving (Show)

lordOfTheRings :: Movie
lordOfTheRings = Movie
  { movieTitle    = "Lord of the Rings"
  , movieDirector = "Peter Jackson"
  , movieCast     = ["Ian McKellen"]
  , movieBudget   = 9999999
  }

-- haskell does not have a notion of constructors, they're just plain functions
indieMovie :: String -> String -> Movie
indieMovie title director = Movie
  { movieTitle    = title
  , movieDirector = director
  , movieCast     = [director]
  , movieBudget   = 0
  }

movieAverageSalary :: Movie -> Double
movieAverageSalary movie =
  let cl = fromIntegral (length (movieCast movie)) :: Double
  in  movieBudget movie / cl

-- since haskell is immutable, the concept of setters does not really exist
-- because you can only create new objects
fireCast :: Movie -> Movie
fireCast m = m { movieCast = [] }


----------------------------------------------------
-- Algebraic Data Types (ADT)
----------------------------------------------------
-- haskell supports another type of data called Algebraic Data Types (aka ADT)
-- that are similar to `enum` with the added bonus that each variant can
-- contain additional data
data MyBool = MyTrue  | MyFalse deriving (Show)

prettyPrintMyBool :: MyBool -> String
prettyPrintMyBool MyTrue  = "True"
prettyPrintMyBool MyFalse = "False"

data MyList a = NonEmpty a (MyList a) | Empty deriving (Show)

-- haskell supports user defined operators with options to customize its
-- parsing behaviour. How cool is that? However, with great power comes great
-- responsibility.
(^-^) :: a -> MyList a -> MyList a
(^-^) e l = NonEmpty e l
infixr 5 ^-^

toList :: MyList a -> [a]
toList Empty          = []
toList (NonEmpty e l) = e : toList l

myListExample = 1 ^-^ 2 ^-^ Empty

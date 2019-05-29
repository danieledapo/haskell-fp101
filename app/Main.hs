import           System.Environment

import           Lib

------------------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------------------
main :: IO ()
main = do
  args     <- getArgs

  contents <- if null args then getContents else readFile (head args)
  let input = filter (`elem` "+-<>.,[]") contents

  _ <- runIO zeroedTape (parse input)
  pure ()

module Main
  ( main
  ) where 

import Data.String        (IsString(..))
import System.Environment (getArgs)
import Utils       (unsafeTokenNameToHex)

{- -------------------------------------------------------------------
   Tool to convert a token name represented as a string, into a
   hexadecimal value. Output can be passed to the Cardano CLI.
-- ----------------------------------------------------------------- -}

main :: IO ()
main = do
  [tn'] <- getArgs
  let tn = fromString tn'
  putStrLn $ unsafeTokenNameToHex tn

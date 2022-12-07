
module Main where

import qualified Spec.Trace as SpecTrace
import           Test.Tasty

main :: IO ()
main = defaultMain SpecTrace.tests

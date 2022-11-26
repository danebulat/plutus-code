module Main
  ( main
  ) where 

import Control.Exception  (throwIO)
import Data.String        (IsString(..))
import System.Environment (getArgs)

import Week06.OnChain     (policy)
import Week06.Utils       (unsafeReadTxOutRef, writeMintingPolicy)


{- -------------------------------------------------------------------
A tool that serialises the week6 minting policy script to a file.
For example, outputting vesting.plutus.
This file can then be passed to the Cardano CLI to execute the tx.
-- ----------------------------------------------------------------- -}

main :: IO ()
main = do
  [file, oref', amt', tn'] <- getArgs
  let oref = unsafeReadTxOutRef oref'
      amt  = read amt'
      tn   = fromString tn'
      p    = policy oref tn amt
  e <- writeMintingPolicy file p
  case e of
    Left err -> throwIO $ userError $ show err
    Right () -> return ()

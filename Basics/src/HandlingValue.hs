{-# LANGUAGE OverloadedStrings #-}

module HandlingValue where 

import qualified Plutus.V1.Ledger.Value as Value
import qualified Ledger.Ada             as Ada

{-
  The CurrencySymbol must be a hex value.
  The TokenName is an arbitrary byte string.
  
  Ledger.Ada
  fromValue       :: Value -> Ada
  toValue         :: Ada -> Value
  lovelaceOf      :: Integer -> Ada 
  adaOf           :: Micro -> Ada
  lovelaceValueOf :: Integer -> Value
  adaValueOf      :: Micro -> Value 
  ...
  
  Plutus.V1.Ledger.Value 
-}

main :: IO ()
main = do
  -- Get the currency symbol for the ADA asset class
  -- adaSymbol :: CurrencySymbol
  print $ "ADA currency symbol: " ++ show Ada.adaSymbol

  -- Get the token name of the ADA asset class
  -- adaToken :: TokenName
  print $ "ADA token name: " ++ show Ada.adaToken

  -- Get an amount of lovelace from an integer
  -- lovelaceValueOf :: Integer -> Value
  print $ "lovelaceValueOf: " ++ show (Ada.lovelaceValueOf 100)

  -- We can combine Value type instances
  print $ Ada.lovelaceValueOf 123 <> Ada.lovelaceValueOf 10

  -- Creating values containing native tokens
  -- singleton :: CurrencySymbol -> TokenName -> Integer -> Value
  print $ Value.singleton "a8ff" "ABC" 7 <>
          Ada.lovelaceValueOf 42 <>
          Value.singleton "a8ff" "XYZ" 100

  -- Given a value, you can extract an amount from an asset class
  let v = Value.singleton "a8ff" "ABC" 7 <>
          Ada.lovelaceValueOf 42 <>
          Value.singleton "a8ff" "XYZ" 100

  print $ "XYZ in Value: " ++ show (Value.valueOf v "a8ff" "XYZ")
  print $ "ABC in Value: " ++ show (Value.valueOf v "a8ff" "ABC")
  print $ "abc in Value: " ++ show (Value.valueOf v "a8ff" "abc")

  -- flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
  print $ "Flattened v: " ++ show (Value.flattenValue v)

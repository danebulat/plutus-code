{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Basics.DataModule where

import PlutusTx
import PlutusTx.Builtins (mkI, mkB, mkList, mkMap)
import PlutusTx.IsData.Class

-- Playing with the Data type
-- Can convert to BuiltinData type for on-chain code


-- ----------------------------------------------------------------------
-- ToData / FromData

-- Make MySillyRedeemer class an instance of ToData and FromData
-- so it can be used in validator scripts.

newtype MySillyRedeemer = MySillyRedeemer Integer

PlutusTx.unstableMakeIsData ''MySillyRedeemer


-- ----------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  -- printing Data
  print $ I 42
  print $ B "bytestring"
  print $ Map [(I 42, B "Haskell"), (List [I 0], I 1000)]
  print $ List [I 22, B "string", Map [(I 2, B "two")]]

  -- printing BuiltinData
  print $ mkI 42
  print $ mkB "bytestring"
  print $ mkList [mkI 1, mkI 2]
  print $ mkMap [(mkI 42, mkB "test")]

  -- converting types to Data type
  -- required when computing a typed validator
  
  print $ toData ()                           -- Constr 0 []
  print (fromData (Constr 0 []) :: Maybe ())  -- Just ()
  print (fromData (Constr 1 []) :: Maybe ())  -- Nothing
  print $ toData (42 :: Integer)              -- I 42
  print (fromData (I 42) :: Maybe Integer)    -- Just 42

  -- :i ToData
  -- :i FromData
  print $ toData (MySillyRedeemer 42)         -- Constr 0 [I 42]
  print $ (fromData (I 42) :: Maybe Integer)  -- Just 42

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Helper.Deploy where

import qualified Cardano.Api           as CApi
import Cardano.Api.Shelley             (PlutusScript(..), PlutusScriptV2)
import Codec.Serialise                 (serialise)
import Data.Aeson                      (encode)
import qualified Data.ByteString.Lazy  as LBS 
import qualified Data.ByteString.Short as SBS
import PlutusTx                        (Data(..))
import qualified Plutus.V2.Ledger.Api  as V2LedgerApi
import qualified Ledger
import qualified PlutusTx

import qualified Parameterized.OnChain as ParamOnChain


-- Convert Plutus Data to Cardano API ScriptData
dataToScriptData :: Data -> CApi.ScriptData
dataToScriptData (Constr n xs) = CApi.ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = CApi.ScriptDataMap [(dataToScriptData x, dataToScriptData y)
                                                 | (x, y) <- xs]
dataToScriptData (List xs)     = CApi.ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = CApi.ScriptDataNumber n
dataToScriptData (B bs)        = CApi.ScriptDataBytes bs

-- Serialise and write Plutus ToData object to a file for Cardano CLI
writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file
               . encode
               . CApi.scriptDataToJson CApi.ScriptDataJsonDetailedSchema
               . dataToScriptData
               . PlutusTx.toData

-- Specialised version of writeJSON to write the unit value
writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()

-- Serialise and write a Plutus Core validator script to file for Cardano CLI 
writeValidator :: FilePath -> V2LedgerApi.Validator -> IO (Either (CApi.FileError ()) ())
writeValidator file = CApi.writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing
                    . PlutusScriptSerialised
                    . SBS.toShort
                    . LBS.toStrict
                    . serialise
                    . V2LedgerApi.unValidatorScript

-- ----------------------------------------------------------------------
-- Specialised functions for contract examples

writeParameterisedDatum :: IO ()
writeParameterisedDatum = writeJSON "testnet/paramDatum.json"
    $ ParamOnChain.Dat { ParamOnChain.dData = 20 }

writeParameterisedRedeemer :: IO ()
writeParameterisedRedeemer = writeJSON "testnet/paramRedeemer.json"
    $ ParamOnChain.Dat { ParamOnChain.dData = 20 }

writeParameterisedValidator :: IO (Either (CApi.FileError ()) ())
writeParameterisedValidator =
  writeValidator "testnet/param.plutus"
               $ ParamOnChain.validator
               $ ParamOnChain.BenParam {
    ParamOnChain.creator     = Ledger.PaymentPubKeyHash
                               "37ed7e6b17eeefaeb01ebbbba49a677be6138336aca8909fc37ea808",
    ParamOnChain.beneficiary = Ledger.PaymentPubKeyHash
                               "1ca9410b9c346768a410f8aa3599ad6ff134864e7381e2cb8c83db0a",
    ParamOnChain.deadline    = 1609251373999
  }

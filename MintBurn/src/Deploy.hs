{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Deploy where 

{-# OPTIONS_GHC -fno-warn-unused-imports         #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns       #-}
{-# OPTIONS_GHC -fnowarn-incomplete-uni-patterns #-}

import qualified Data.ByteString.Char8    as B
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString.Short    as SBS
import qualified Data.ByteString.Base16   as B16

import           Cardano.Api
import           Cardano.Api.Shelley      (PlutusScript (..))
import           Codec.Serialise          (serialise)
import qualified Data.Aeson               as DataAeson
import qualified Plutus.V2.Ledger.Api     as LedgerApiV2
import qualified PlutusTx
import qualified Ledger
import qualified PlutusTx.Prelude         as PlutusPrelude 
import qualified Data.String              as DataString (IsString(fromString))
import qualified Plutus.V1.Ledger.Scripts as ScriptsLedger
import qualified OnChain

--------------------------------------------------------------------
-- Default TokenParam 
--------------------------------------------------------------------

-- Not instance of Default to avoid an orphan instance
defTokenParam :: OnChain.TokenParam
defTokenParam = OnChain.TokenParam 
        { OnChain.utxo   = convertToUtxo utxo 
        , OnChain.name   = convertToTokenName tokenName
        , OnChain.minter = convertToPubkeyHash minter 
        }

--------------------------------------------------------------------
-- Section to insert the Paramemters to initialize the contract
--------------------------------------------------------------------

minter :: B.ByteString
minter = "80b34df2162e9c4a38ce63322a8f903c9455a0bebd64c02cf1f3222a" 

tokenName :: B.ByteString
tokenName = "MyTestEMG"

utxo :: String 
utxo = "f3db80d707e9df4bb6edf9b267839e0953474fd006e76e8ddeb125d057599884#0"

parameters :: OnChain.TokenParam 
parameters = OnChain.TokenParam 
  { OnChain.utxo   = convertToUtxo utxo 
  , OnChain.name   = convertToTokenName tokenName 
  , OnChain.minter = convertToPubkeyHash minter
  }

--------------------------------------------------------------------
-- Helper Functions (for MintBurn Example)
--------------------------------------------------------------------

decodeHex :: B.ByteString -> PlutusPrelude.BuiltinByteString
decodeHex hexBS = 
  case getTx of
    Right decHex -> do 
      PlutusPrelude.toBuiltin decHex 
    Left _ -> do 
      PlutusPrelude.emptyByteString
  where 
    getTx :: Either String B.ByteString 
    getTx = B16.decode hexBS

convertToPubkeyHash :: B.ByteString -> Ledger.PaymentPubKeyHash
convertToPubkeyHash b = Ledger.PaymentPubKeyHash (Ledger.PubKeyHash $ decodeHex b)

convertToTokenName :: B.ByteString -> Ledger.TokenName
convertToTokenName b = LedgerApiV2.TokenName (LedgerApiV2.toBuiltin b)

convertToUtxo :: String -> LedgerApiV2.TxOutRef
convertToUtxo s = 
  let 
    (x, _ : y) = span (/= '#') s 
  in 
    LedgerApiV2.TxOutRef 
      {
        LedgerApiV2.txOutRefId  = DataString.fromString x,
        LedgerApiV2.txOutRefIdx = read y
      }

--------------------------------------------------------------------
-- Helper Functions (Validator)
--------------------------------------------------------------------

dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs) = 
    ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs)      = 
    ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n)         = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = 
    LBS.writeFile file 
  . DataAeson.encode 
  . scriptDataToJson ScriptDataJsonDetailedSchema 
  . dataToScriptData 
  . PlutusTx.toData

writeValidator :: FilePath -> LedgerApiV2.MintingPolicy -> IO (Either (FileError ()) ())
writeValidator file = 
    writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing 
  . PlutusScriptSerialised 
  . SBS.toShort 
  . LBS.toStrict 
  . serialise 
  . LedgerApiV2.unMintingPolicyScript

--------------------------------------------------------------------
-- MintBurn CLI Artefacts
--------------------------------------------------------------------

writeDatumUnit :: IO ()
writeDatumUnit = writeJSON "src/MintBurn/Deploy/unit.json" ()

writeRedeemerMint :: IO () 
writeRedeemerMint = writeJSON "src/MintBurn/Deploy/redeemer-mint.json" mint
  where 
    mint = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData OnChain.Mint 

writeMintBurn :: IO (Either (FileError ()) ())
writeMintBurn = 
  writeValidator "src/MintBurn/Deploy/MintBurn.plutus" $ OnChain.policy parameters

writeRedeemerBurn :: IO ()
writeRedeemerBurn = writeJSON "src/MintBurn/Deploy/redeemer-burn.json" burn
  where 
    burn = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData OnChain.Burn

--------------------------------------------------------------------
-- Main
--------------------------------------------------------------------

main :: IO ()
main = do 
  writeDatumUnit
  writeRedeemerMint
  writeRedeemerBurn
  _ <- writeMintBurn

  return ()

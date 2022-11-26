{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Week06.Utils where

import Cardano.Api
import Cardano.Api.Shelley             (PlutusScript (..), Address(..))
import Codec.Serialise (serialise)
import qualified Data.Aeson            as DataAeson
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Char8 as BS8
import qualified Plutus.V2.Ledger.Api  as LedgerApiV2
import qualified PlutusTx
import PlutusTx.Builtins.Internal      (BuiltinByteString(..))
import qualified Ledger                as PlutusLedger

import Cardano.Crypto.Hash.Class       (hashToBytes)
import Cardano.Ledger.Credential       as Ledger
import Cardano.Ledger.Crypto           (StandardCrypto)
import Cardano.Ledger.Hashes           (ScriptHash(..))
import Cardano.Ledger.Keys             (KeyHash(..))
import Data.Text                       (pack)
import Data.Maybe                      (fromMaybe, fromJust)
import Data.String                     (fromString)


-- ----------------------------------------------------------------------
-- Plutus Data to Cardano API ScriptData

dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs)
  = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n)         = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs)        = ScriptDataBytes bs


-- ----------------------------------------------------------------------
-- Write Plutus toData instance to JSON file (for Cardano CLI)

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file
               . DataAeson.encode
               . scriptDataToJson ScriptDataJsonDetailedSchema
               . dataToScriptData . PlutusTx.toData


-- ----------------------------------------------------------------------
-- Write unit file

writeUnitToFile :: FilePath -> IO ()
writeUnitToFile file = writeJSON file ()

-- Serialised version (JSON file) of unit which we provide as the datum
-- and redeemer to the cardano-cli.
writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()


-- ----------------------------------------------------------------------
-- Serialise a Plutus Core minting poliy script

writeValidator :: FilePath -> LedgerApiV2.MintingPolicy -> IO (Either (FileError ()) ())
writeValidator file =
    writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing
  . PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  . serialise
  . LedgerApiV2.unMintingPolicyScript


-- (Older version)
-- Serialise minting policy script and write to file (serialise to disk)
-- Output file can be specified when using the cardano-cli.

writeMintingPolicy :: FilePath -> LedgerApiV2.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicy file =
    writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing
  . PlutusScriptSerialised        -- wrap ShortByteString in this constructor
  . SBS.toShort
  . LBS.toStrict
  . serialise                     -- Script to ByteString
  . LedgerApiV2.getMintingPolicy  -- unwrap Script


-- ----------------------------------------------------------------------
-- Converts a string formatted as <TxHash>#<TxIx> into a TxIx
-- structure, which will reference that UTXO using the Plutus API.

unsafeReadTxOutRef :: String -> LedgerApiV2.TxOutRef
unsafeReadTxOutRef s =
  let
    (x, _ : y) = span (/= '#') s
  in
    LedgerApiV2.TxOutRef
        { LedgerApiV2.txOutRefId  = fromString x  -- String to TxId
        , LedgerApiV2.txOutRefIdx = read y        -- read to Integer
        }


-- ----------------------------------------------------------------------
-- Return the hex value of a TokenName
-- Provide hex name when using the cardano-cli.

-- AssetName corresponds to TokenName type in Plutus.
unsafeTokenNameToHex :: LedgerApiV2.TokenName -> String
unsafeTokenNameToHex =
    BS8.unpack                           -- ByteString to String
  . serialiseToRawBytesHex               -- turn into hexadecimal bytestring (Cardano API)
  . fromJust                             -- unwrap Just
  . deserialiseFromRawBytes AsAssetName  -- deserialise into AssetName
  . getByteString                        -- BuiltinByteString to ByteString
  . LedgerApiV2.unTokenName              -- unwrap BuiltinByteString 
  where
    getByteString (BuiltinByteString bs) = bs


-- -------------------------------------------------------------------
-- getCredentials: Extracts the public key hashes contained in
-- a non-script address. Returns nothing if any such script or
-- pointer is involved, otherwise return the pkh and optional
-- staking pkh.

getCredentials
    :: LedgerApiV2.Address
    -> Maybe (PlutusLedger.PaymentPubKeyHash, Maybe PlutusLedger.StakePubKeyHash)
getCredentials (LedgerApiV2.Address x y) = case x of
    -- Nothing if it's a script address 
    LedgerApiV2.ScriptCredential _   -> Nothing  
    -- Contains a PubKeyHash (it's a public key address)
    LedgerApiV2.PubKeyCredential pkh -> 
      let
        -- PubKeyHash to PaymentPubKeyHash
        ppkh = PlutusLedger.PaymentPubKeyHash pkh
      in
        case y of
            -- Return address pkh (no staking component)
            Nothing -> Just (ppkh, Nothing)
            
            -- Nothing if overall result is a pointer
            Just (LedgerApiV2.StakingPtr _ _ _) -> Nothing

            -- Pattern match the staking pub key hash
            Just (LedgerApiV2.StakingHash h) -> case h of
                -- Nothing if its a validator hash (script)
                LedgerApiV2.ScriptCredential _    -> Nothing
                -- Return the pkh and staking pkh
                LedgerApiV2.PubKeyCredential pkh' ->
                  Just (ppkh, Just $ PlutusLedger.StakePubKeyHash pkh') 


-- Uses `getCredentials` to extract a payment pkh
unsafePaymentPubKeyHash :: PlutusLedger.Address -> PlutusLedger.PaymentPubKeyHash
unsafePaymentPubKeyHash addr = maybe (error $ "script address "
                            ++ show addr
                            ++ " does not contain a payment key") fst $ getCredentials addr


-- Uses `getCredentials` to extract a staking pkh
unsafeStakePubKeyHash :: PlutusLedger.Address -> PlutusLedger.StakePubKeyHash
unsafeStakePubKeyHash addr = case getCredentials addr of
    Nothing           -> error $ "unexpected script address " ++ show addr
    Just (_, Nothing) -> error $ "addres " ++ show addr ++ " contains no stake component"
    Just (_, Just x)  -> x

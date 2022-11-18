module Helper.Compiler where 

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Short  as SBS
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as Base16

-- Cardano modules
import Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)

-- Plutus modules
import qualified Plutus.V2.Ledger.Api   as LedgerApiV2
import qualified Cardano.Binary         as CBOR

-- Our validators
import qualified Simple.OnChain         as Simple


-- ----------------------------------------------------------------------
-- Functions

script :: LedgerApiV2.Script
script = LedgerApiV2.unValidatorScript Simple.validator

scriptShortBs :: SBS.ShortByteString
scriptShortBs = (SBS.toShort . LBS.toStrict . serialise) script

srlScript :: PlutusScript PlutusScriptV2
srlScript = PlutusScriptSerialised scriptShortBs

scriptCBORHex :: B.ByteString
scriptCBORHex = Base16.encode $ CBOR.serialize' srlScript

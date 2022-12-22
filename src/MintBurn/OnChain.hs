{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module MintBurn.OnChain where 

import PlutusTx 
import PlutusTx.Prelude 
import qualified Ledger                                          as L
import qualified Ledger.Address                                  as V1A
import qualified Plutus.V2.Ledger.Api                            as LV2
import qualified Plutus.V2.Ledger.Contexts                       as Ctx
import qualified Plutus.Script.Utils.V2.Scripts                  as UtilsScriptsV2
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UTS
import qualified Plutus.Script.Utils.V2.Typed.Scripts            as Scripts
import qualified Prelude                                         as P
import qualified Plutus.V1.Ledger.Interval                       as I
import qualified Plutus.V1.Ledger.Value                          as V
import qualified Ledger.Ada                                      as Ada
import qualified Ledger

-- ---------------------------------------------------------------------- 
-- Parameter
-- ---------------------------------------------------------------------- 

data TokenParam = TokenParam 
  { utxo   :: !LV2.TxOutRef
  , name   :: !LV2.TokenName 
  , minter :: !L.PaymentPubKeyHash
  } deriving P.Show

PlutusTx.makeIsDataIndexed ''TokenParam[('TokenParam, 0)]
PlutusTx.makeLift ''TokenParam

-- ---------------------------------------------------------------------- 
-- Redeemer 
-- ---------------------------------------------------------------------- 

data Action = Mint | Burn

PlutusTx.makeIsDataIndexed ''Action[('Mint, 0), ('Burn, 1)]

-- ---------------------------------------------------------------------- 
-- Minting Policy Script 
-- ---------------------------------------------------------------------- 

{-# INLINABLE mkPolicy #-}
mkPolicy :: TokenParam ->  Action -> Ctx.ScriptContext -> Bool 
mkPolicy param action ctx = case action of 
    Mint ->
      let 
        hasUtxo :: Bool 
        hasUtxo = any (\i -> Ctx.txInInfoOutRef i == utxo param) $ Ctx.txInfoInputs txInfo

        checkMintedAmount :: Bool 
        checkMintedAmount = case V.flattenValue $ Ctx.txInfoMint txInfo of 
          [(_, tn', amount)] -> tn' == name param && amount == 1 
          other              -> False
      in
        traceIfFalse "Utxo not equal"       hasUtxo           && 
        traceIfFalse "Wrong amount minted"  checkMintedAmount &&
        traceIfFalse "Not signed by minter" checkTxSignedByMinter

    Burn ->
      let 
        checkBurntAmount :: Bool 
        checkBurntAmount = case V.flattenValue $ Ctx.txInfoMint txInfo of 
          [(_, tn', amount)] -> tn' == name param && amount == -1 
          other              -> False
      in 
        traceIfFalse "Wrong amount burnt"   checkBurntAmount &&
        traceIfFalse "Not signed by minter" checkTxSignedByMinter
  where
    txInfo :: Ctx.TxInfo 
    txInfo = Ctx.scriptContextTxInfo ctx

    checkTxSignedByMinter :: Bool 
    checkTxSignedByMinter = Ctx.txSignedBy txInfo (L.unPaymentPubKeyHash $ minter param)

-- ---------------------------------------------------------------------- 
-- Boilerplate
-- ---------------------------------------------------------------------- 

wrappedPolicy :: TokenParam -> BuiltinData -> BuiltinData -> ()
wrappedPolicy param r c = check 
  (mkPolicy param (PlutusTx.unsafeFromBuiltinData r)
  (PlutusTx.unsafeFromBuiltinData c)) 

policy :: TokenParam -> LV2.MintingPolicy
policy tokenParam = LV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrappedPolicy ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode tokenParam

curSymbol :: TokenParam -> LV2.CurrencySymbol
curSymbol = UtilsScriptsV2.scriptCurrencySymbol . policy

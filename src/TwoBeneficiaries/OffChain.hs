{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-} -- To allow notation like GrabParams {..}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module TwoBeneficiaries.OffChain where


import qualified Control.Monad             as Monad (void, unless)
import qualified GHC.Generics              as GHCGenerics (Generic)
import qualified Data.Aeson                as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema       as DataOpenApiSchema (ToSchema)
import qualified Prelude                   as P
import qualified Data.Void                 as Void (Void)
import qualified Data.Map                  as Map
import Data.Text                           (Text, replicate)
import qualified Data.Text                 as T
import Text.Printf                         (printf)
                                           
import qualified PlutusTx                  
import PlutusTx.Prelude                    
import qualified Plutus.Contract           as PlutusContract
import qualified Ledger.Ada                as Ada
import qualified Ledger.Tx                 as LedgerTx
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Ledger                    (PaymentPubKeyHash, getCardanoTxId, unitRedeemer)
import qualified Ledger.Constraints        as Constraints
import qualified Plutus.V1.Ledger.Scripts  as ScriptsLedger
import qualified Plutus.V1.Ledger.Interval as LedgerIntervalV1

import qualified TwoBeneficiaries.OnChain  as OnChain


-- ----------------------------------------------------------------------
-- Data types

data GiveParams = GiveParams
  { gpBeneficiary :: !Ledger.PaymentPubKeyHash
  , gpDeadline    :: !LedgerApiV2.POSIXTime
  , gpAmount      :: !Integer
  } deriving (GHCGenerics.Generic,
              DataAeson.ToJSON,
              DataAeson.FromJSON,
              DataOpenApiSchema.ToSchema)


-- ----------------------------------------------------------------------
-- Give endpoint

give :: forall w s e. PlutusContract.AsContractError e
     => GiveParams
     -> PlutusContract.Contract w s e ()
give gp = do
    pkh <- PlutusContract.ownFirstPaymentPubKeyHash

    let dat     = OnChain.Dat {
                    OnChain.beneficiary1 = gpBeneficiary gp,
                    OnChain.beneficiary2 = pkh,
                    OnChain.deadline     = gpDeadline gp
                  }
              
        lookups = Constraints.plutusV2OtherScript OnChain.validator
        tx      = Constraints.mustPayToOtherScript
                    (OnChain.validatorHash)
                    (LedgerApiV2.Datum   $ PlutusTx.toBuiltinData dat)
                    (Ada.lovelaceValueOf $ gpAmount gp)
    
    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.VTypes lookups tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx


-- ----------------------------------------------------------------------
-- Grab endpoint

grab :: forall w s e. PlutusContract.AsContractError e
     => PlutusContract.Contract w s e ()
grab = do
    now   <- PlutusContract.currentTime
    pkh   <- PlutusContract.ownFirstPaymentPubKeyHash
    utxos <- PlutusContract.utxosAt OnChain.address

    let utxos1 = Map.filter (isSuitable $ \dat ->
                   OnChain.beneficiary1 dat == pkh && now <= OnChain.deadline dat) utxos
        utxos2 = Map.filter (isSuitable $ \dat ->
                   OnChain.beneficiary2 dat == pkh && now > OnChain.deadline dat) utxos

    unless (Map.null utxos1) $ spendUtxos utxos1 now False
    unless (Map.null utxos2) $ spendUtxos utxos2 now True
  where
    isSuitable :: (OnChain.Dat -> Bool) -> LedgerTx.ChainIndexTxOut -> Bool
    isSuitable p o = let (_, mDat) = LedgerTx._ciTxOutScriptDatum o in
      case mDat of
        Nothing  -> False
        Just dat -> let maybeDat = PlutusTx.fromBuiltinData (LedgerApiV2.getDatum dat)
                    in maybe False p maybeDat


-- ----------------------------------------------------------------------
-- Spend the suitable UTXOs

spendUtxos :: forall w s e. PlutusContract.AsContractError e
           => Map.Map LedgerTx.TxOutRef LedgerTx.ChainIndexTxOut
           -> LedgerApiV2.POSIXTime
           -> Bool
           -> PlutusContract.Contract w s e ()
spendUtxos utxos now b = do
  let orefs     = fst <$> Map.toList utxos
      interval' = if b then LedgerIntervalV1.to now else LedgerIntervalV1.from now
      lookups   = Constraints.unspentOutputs utxos P.<>
                  Constraints.plutusV2OtherScript OnChain.validator

      tx :: Constraints.TxConstraints () OnChain.Dat
      tx = mconcat [Constraints.mustSpendScriptOutput oref Ledger.unitRedeemer
                   | oref <- orefs] P.<>
                   Constraints.mustValidateIn interval'
  Monad.void $ PlutusContract.submitTxConstraintsWith @OnChain.VTypes lookups tx


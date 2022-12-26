#!/bin/bash

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 1 \
  --tx-in <SignerTxHash>#<TxIx> \
  --tx-in <ScriptTxHash>#<TxIx> \
  --tx-in-script-file vesting.plutus \
  --tx-in-datum-file vestingDatum.json \
  --tx-in-redeemer-file vestingRedeemer.json \
  --tx-in-collateral <TxHash>#<TxIx> \
  --change-address $(cat 01.addr) \
  --invalid-before <slot> \
  --required-signer-hash <pkh> \
  --protocol-params-file protocol.json \
  --out-file tx2.body

cardano-cli transaction sign \
  --tx-body-file tx2.body \
  --signing-key-file 01.skey \
  --testnet-magic 1 \
  --out-file tx2.signed

cardano-cli transaction submit \
  --tx-file tx2.signed \
  --testnet-magic 1

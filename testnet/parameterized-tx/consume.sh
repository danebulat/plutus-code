#!/bin/bash

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 1 \
  --required-signer-hash <beneficiary-pkh> \
  --change-address <creator-addr> \
  --tx-in <TxHash>#<TxIx> \
  --tx-in-script-file param.plutus \
  --tx-in-datum-file paramDatum.json \
  --tx-in-redeemer-file paramRedeemer.json \
  --tx-in-collateral <TxHash>#<TxIx> \
  --tx-out <creator-addr>+<10%-deposit> \
  --invalid-before <slot> \
  --protocol-params-file protocol.json \
  --out-file tx2.body

cardano-cli transaction sign \
  --tx-body-file tx2.body \
  --signing-key-file <beneficiary-skey-file> \
  --testnet-magic 1 \
  --out-file tx2.signed

cardano-cli transaction submit \
  --tx-file tx2.signed \
  --testnet-magic 1

# Vesting Cardano CLI Transactions

## Contract Overview

A producer deposits some value in a UTXO under the script's address. A
beneficiary payment public key hash and deadline are specified in the 
datum. 

The **beneficiary payment public key hash** specifies who is allowed to 
unlock the script output UTXO.

The **deadline** specifies from when the deposited funds are allowed to 
be unlocked by the beneficiary.

## Generate Script Address 

Deprecated method:

```
cardano-cli address build-script \
  --script-fiile vesting.plutus \
  --testnet-magic 1 \
  --out-file vesting.addr
```

New method:

```
cardano-cli address built \
  --payment-script-file vesting.plutus \
  --testnet-magic 1 \
  --out-file vesting.addr
```

## Producing Tx

```
cardano-cli transaction build \
  --babbage-era \
  --change-address <addr> \
  --tx-in <TxHash>#<TxIx> \
  --tx-out <script-addr>+10000000 \
  --tx-out-datum-hash-file vestingDatum.json \
  --out-file tx.body \
  --testnet-magic 1

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file payment.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --tx-file tx.signed \
  --testnet-magic 1
```

- View UTXOs of script address to confirm that the transaction was 
  successful.

## Consuming Tx

```
cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 1 \
  --tx-in <SignerTxHash>#<TxIx> \              # for tx fees
  --tx-in <ScriptTxHash>#<TxIx> \              # script utxo to spend
  --tx-in-script-file vesting.plutus \
  --tx-in-datum-file vestingDatum.json \
  --tx-in-redeemer-file vestingRedeemer.json \
  --tx-in-collateral <TxHash>#<TxIx> \         # ada utxo
  --change-address $(cat 01.addr) \            # all value sent to this address
  --invalid-before <slot> \
  --required-signer-hash <pkh> \               # requried signer explicitly stated
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
```

- View UTXOs of the script and beneficiary to confirm that the 
  transaction was successful.

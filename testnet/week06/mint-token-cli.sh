#!/bin/bash 

# utxo reference to spend
oref=$1
# amount of native token to mint
amt=$2
# token name to mint
tn=$3
# address to store minted tokens
addrFile=$4
# signing key file for address
skeyFile=$5

echo "oref: $oref"
echo "amt: $amt"
echo "tn: $tn"
echo "address file: $addrFile"
echo "signing key file: $skeyFile"

# protocol parameters file
ppFile=protocol-parameters.json
cardano-cli query protocol-parameters --testnet-magic 2 --out-file $ppFile

# write plutus serialised minting policy (using token-policy.hs)
pathPrefix=testnet/week06
policyFile=token.plutus
cd ../..
cabal exec token-policy -- $pathPrefix/$policyFile $oref $amt $tn

# file path to tx.unsigned
unsignedFile=tx.unsigned
# file path to tx.signed
signedFile=tx.signed
# polictID or currenct symbol of native token (can use Haskell or cardano-cli)
pid=$(cardano-cli transaction policyid --script-file $pathPrefix/$policyFile)
# convert token name to hex value (using token-name.hs)
tnHex=$(cabal exec token-name -- $tn)
# address to store minted native tokens
addr=$(cat $addrFile)
# value to mint (amount policyId.tokenName)
v="$amt $pid.$tnHex"

echo "currency symbol: $pid"
echo "token name (hex): $tnHex"
echo "minted value: $v"
echo "address: $addr"

cd ./testnet/week06

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 2 \
  --tx-in $oref \
  --tx-in-collateral $oref \
  --tx-out $addr+1500000+"$v" \
  --mint "$v" \
  --minting-script-file $policyFile \
  --mint-redeemer-file unit.json \
  --change-address $addr \
  --protocol-params-file $ppFile \
  --out-file tx.unsigned

cardano-cli transaction sign \
  --tx-body-file tx.unsigned \
  --signing-key-file $skeyFile \
  --testnet-magic 2 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 2 \
  --tx-file $signedFile

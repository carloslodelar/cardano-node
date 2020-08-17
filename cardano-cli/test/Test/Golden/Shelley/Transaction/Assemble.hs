{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Transaction.Assemble
  ( golden_shelleyTransactionAssemble
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

{- HLINT ignore "Use camelCase" -}

-- Check that we can assemble a txbody and a tx witness (multisig script) to form a transaction

golden_shelleyTransactionAssemble :: Property
golden_shelleyTransactionAssemble = propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
  -- Use the same (faked) TxIn for both transactions.
  let txIn = "2392d2b1200b5139fe555c81261697b29a8ccf561c5c783d46e78a479d977053#0"

  -- Using a Shelley output address
  let txOut = "addr1q94cxl99qvtwunsqqv6g9mgj3zrawtpt4edsgwxkjtwpy5dsezcht90tmwfur7t5hc9fk8hjd3r5vjwec2h8vmk3xh8s7er7t3+100"

  txBodyOutFile <- noteTempFile tempDir "tx-body-out"

  void $ execCardanoCLI
    [ "shelley","transaction","build-raw"
    , "--tx-in", txIn
    , "--tx-out", txOut
    , "--ttl", "60"
    , "--fee", "12"
    , "--tx-body-file", txBodyOutFile
    ]

  assertFileOccurences 1 "TxUnsignedShelley" txBodyOutFile
  assertEndsWithSingleNewline txBodyOutFile



  -- Test assembling an all multisig script
  allTxOutFile <- noteTempFile tempDir "all-tx-out"
  allScriptWitnessFile <- noteInputFile "test/data/golden/shelley/multisig/scripts/all"
  void $ execCardanoCLI
    [ "shelley","transaction","sign-witness"
    , "--tx-body-file", txBodyOutFile
    , "--script-file", allScriptWitnessFile
    , "--out-file", allTxOutFile
    ]

  assertFileOccurences 1 "TxSignedShelley" allTxOutFile

  -- Test assembling an any multisig script
  anyTxOutFile <- noteTempFile tempDir "any-tx-out"
  anyScriptWitnessFile <- noteInputFile "test/data/golden/shelley/multisig/scripts/any"
  void $ execCardanoCLI
    [ "shelley","transaction","sign-witness"
    , "--tx-body-file", txBodyOutFile
    , "--script-file", anyScriptWitnessFile
    , "--out-file", anyTxOutFile
    ]

  assertFileOccurences 1 "TxSignedShelley" anyTxOutFile

  -- Test assembling an atleast multisig script
  atLeastTxOutFile <- noteTempFile tempDir "atleast-tx-out"
  atLeastScriptWitnessFile <- noteInputFile "test/data/golden/shelley/multisig/scripts/atleast"
  void $ execCardanoCLI
    [ "shelley","transaction","sign-witness"
    , "--tx-body-file", txBodyOutFile
    , "--script-file", atLeastScriptWitnessFile
    , "--out-file", atLeastTxOutFile
    ]

  assertFileOccurences 1 "TxSignedShelley" atLeastTxOutFile

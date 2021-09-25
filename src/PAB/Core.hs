{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- The intake script bundles incoming UTxOs into batches to then
-- create transactions. A transaction is first created by the person
-- and sent to the address that is checked by the intake script. The
-- intake script then creates a transaction to send everything back to
-- the recipients via a Batch. When a Batch is filled up, the Intake
-- then creates a new Batch and the cycle repeats. The intake script
-- can have an off-chain code part that locates the Batch and then
-- creates the Tx

-- The Trinkets should come in and then be melted to gold. The
-- trinkets need to be burned. The gold then needs to be minted. There
-- should be a minting policy that only allows gold to be burned if
-- trinkets are minted.

-- Another way to do it would be to delegate transactions in real time
-- to new Intake scripts that then create new batches. 

module PAB.Core
    ( Intake (..)
    , IntakeRedeemer (..)
    , intakeTokenName
    , intakeValue
    , intakeAsset
    , typedIntakeValidator
    , intakeValidator
    , intakeAddress
    , IntakeSchema
    , IntakeParams (..)
    , runIntake
    , findIntake
    , Batch (..)
    , BatchRedeemer (..)
    , batchTokenName
    , batchValue
    , batchAsset
    , typedBatchValidator
    , batchValidator
    , batchAddress
    , BatchSchema
    , BatchParams (..)
    , runBatch
    , findBatch
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude

data Intake = Intake
  { iSymbol   :: !CurrencySymbol
    , iOperator :: !PubKeyHash
    , iFee      :: !Integer
    , iAsset    :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Batch

data Batch = Batch {
  { bSymbol   :: !CurrencySymbol
    , bOperator :: !PubKeyHash
    , bFee      :: !Integer
    , bAsset    :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Batch

data IntakeRedeemer = Use
    deriving Show

data BatchRedeemer = Use
    deriving Show

PlutusTx.unstableMakeIsData

{-# INLINABLE intakeTokenName #-}
intakeTokenName :: TokenName
intakeTokenName = TokenName $ toByteString "intake"

{-# INLINABLE intakeValue #-}
intakeValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
intakeValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE batchTokenName #-}
batchTokenName :: TokenName
batchTokenName = TokenName $ toByteString "batch"

{-# INLINABLE batchValue #-}
batchValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
batchValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkIntakeValidator #-}
mkIntakeValidator :: Intake -> Integer -> IntakeRedeemer -> ScriptContext -> Bool
mkIntakeValidator intake x r ctx =
    traceIfFalse "token missing from input"  inputHasToken  &&
    traceIfFalse "token missing from output" outputHasToken &&
    case r of
        Make ->   traceIfFalse "operator signature missing" (txSignedBy info $ iOperator intake) && 
                  traceIfFalse "invalid output datum"       validOutputDatum
        Use    -> traceIfFalse "intake value changed"       (outputDatum == Just x)              &&
                  traceIfFalse "fees not paid"              feesPaid &&
                  traceIfFalse "too small or need time"     readyToSend
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "intake input missing"
        Just i  -> txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (intakeAsset intake) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one intake output"

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (intakeAsset intake) == 1

    outputDatum :: Maybe Integer
    outputDatum = intakeValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    readyToSend :: Bool
    readyToSend = 

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee intake))

data Oracling
instance Scripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = IntakeRedeemer

typedIntakeValidator :: Intake -> Scripts.TypedValidator Oracling
typedIntakeValidator intake = Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkIntakeValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode intake)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @IntakeRedeemer

{-# INLINABLE mkBatchValidator #-}
mkBatchValidator :: Batch -> Integer -> BatchRedeemer -> ScriptContext -> Bool
mkBatchValidator batch x r ctx =
    traceIfFalse "token missing from input"  inputHasToken  &&
    traceIfFalse "token missing from output" outputHasToken &&
    case r of
        Make -> traceIfFalse "operator signature missing" (txSignedBy info $ bOperator batch) && 
                  traceIfFalse "invalid output datum"       validOutputDatum
        Use    -> traceIfFalse "batch value changed"       (outputDatum == Just x)              &&
                  traceIfFalse "fees not paid"              feesPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "batch input missing"
        Just i  -> txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (batchAsset batch) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one batch output"

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (batchAsset batch) == 1

    outputDatum :: Maybe Integer
    outputDatum = batchValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee batch))

data Oracling
instance Scripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = BatchRedeemer

typedBatchValidator :: Batch -> Scripts.TypedValidator Oracling
typedBatchValidator batch = Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkBatchValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode batch)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @BatchRedeemer

-- we have set it up so that we can call an intake and a batch, and
-- the rest below finishes that up. The client will have to put their
-- requests on on-chain with a request token, and then the intake will
-- scoop up all of these and send them to batches. The scoop and out
-- goes to the batches. The batches can then read the datum, which
-- will tell the batch to whom to send the transaction.

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . typedOracleValidator

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator

data OracleParams = OracleParams
    { opFees   :: !Integer
    , opSymbol :: !CurrencySymbol
    , opToken  :: !TokenName
    } deriving (Show, Generic, FromJSON, ToJSON)

startOracle :: forall w s. OracleParams -> Contract w s Text Oracle
startOracle op = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    osc <- mapError (pack . show) (mintContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs     = Currency.currencySymbol osc
        oracle = Oracle
            { oSymbol   = cs
            , oOperator = pkh
            , oFee      = opFees op
            , oAsset    = AssetClass (opSymbol op, opToken op)
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (typedOracleValidator oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.typedValidatorLookups (typedOracleValidator oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x

-- New Code

newOracle :: forall w s. Oracle -> Integer -> Contract w s Text ()
newOracle oracle x = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (typedOracleValidator oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.typedValidatorLookups (typedOracleValidator oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x

findOracle :: forall w s. Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
findOracle oracle = do
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
            return (oref, o, x)
        _           -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1

type OracleSchema = Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update"
        updateOracle oracle x
        go oracle

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module PAB.RequestGoldScript
  ( apiGoldScript
  , apiRequestScript
  , GoldParams (..)
  , typedGoldValidator
  , goldValidator
  , typedRequestValidator
  , requestValidator
  ) where

import           Cardano.Api.Shelley         (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy        as LB
import qualified Data.ByteString.Short       as SBS
import           Ledger                      hiding (singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Ledger.Ada
import qualified PlutusTx
import           PlutusTx.Builtins           (modInteger)
import           PlutusTx.Prelude            hiding (Semigroup (..), unless)
import           Plutus.V1.Ledger.Bytes      (bytes)
import qualified Plutus.V1.Ledger.Scripts    as Plutus
import           Plutus.V1.Ledger.Credential (Credential (..))
import           Prelude                     (Show)

data GoldParams = GoldParams
    { gpSeed      :: !Integer
    , gpNFT       :: !AssetClass
    , gpCounter   :: !AssetClass
    , gpVotes     :: !AssetClass
    , gpNameCount :: !Integer
    , gpVoteCount :: !Integer
    , gpFee       :: !Integer
    } deriving Show

PlutusTx.makeLift ''GoldParams

{- HLINT ignore "Avoid lambda" -}

mkRequestValidator :: GoldParams -> Integer -> Integer -> ScriptContext -> Bool
mkRequestValidator gp _ _ ctx = any (\i -> assetClassValueOf (txOutValue $ txInInfoResolved i) (gpNFT gp) == 1) $ txInfoInputs $ scriptContextTxInfo ctx
    -- Check whether the NFT is present in an input of the transaction beeing validated. That NFT "sits" in the state carrying UTxO of the main gold contract,
    -- We can therefore be sure that the main gold validator will be executed.

data Requesting
instance Scripts.ValidatorTypes Requesting where
    type instance DatumType Requesting = Integer
    type instance RedeemerType Requesting = Integer

typedRequestValidator :: GoldParams -> Scripts.TypedValidator Requesting
typedRequestValidator gp = Scripts.mkTypedValidator @Requesting
    ($$(PlutusTx.compile [|| mkRequestValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode gp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @Integer

requestValidator :: GoldParams -> Validator
requestValidator = Scripts.validatorScript . typedRequestValidator

requestScript :: GoldParams -> Plutus.Script
requestScript = Ledger.unValidatorScript . requestValidator

requestScriptAsShortBs :: GoldParams -> SBS.ShortByteString
requestScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . requestScript

apiRequestScript :: GoldParams -> PlutusScript PlutusScriptV1
apiRequestScript = PlutusScriptSerialised . requestScriptAsShortBs

expectedDatumHash :: DatumHash
expectedDatumHash = DatumHash $ toBuiltin $ bytes "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314" -- hash of 0

data ResultAcc = ResultAcc !Integer !Integer

instance Eq ResultAcc where
  {-# INLINABLE (==) #-}
  ResultAcc v c == ResultAcc v' c' = (v == v') && (c == c')

mkGoldValidator :: DatumHash -> GoldParams -> Integer -> BuiltinData -> ScriptContext -> Bool
mkGoldValidator h gp _ _ ctx
  | oldNFT == 1 =                              -- Are we validating
                                               -- the "special" UTxO
                                               -- carrying the state?
      (txOutDatumHash ownOutput == Just h)  && -- The datum of the
                                               -- "updated" UTxO
                                               -- should be 0.
      (newNFT == 1)                         && -- The "updated" UTxO
                                               -- must contain the
                                               -- NFT.
      validCounters

  | otherwise   = True                         -- If we don't have the
                                               -- UTxO with the NFT,
                                               -- we don't care.

  where
    ctxInfo :: TxInfo
    ctxInfo = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput =
      let in_l = txInfoInputs $ ctxInfo in
       case getScriptInput in_l of
        Nothing -> traceError "gold input missing"
        Just o  -> o

    getScriptInput :: [TxInInfo] -> Maybe TxOut
    getScriptInput [] = Nothing
    getScriptInput (i : tl)
      | txInInfoOutRef i == ownRef = Just (txInInfoResolved i)
      | otherwise                  = getScriptInput tl

    ownOutput :: TxOut
    ownOutput =
      let out_l = txInfoOutputs $ ctxInfo in
        case getScriptOutputs out_l [] of
        [o] -> o
        _   -> traceError "expected exactly one gold output"

    getScriptOutputs :: [TxOut] -> [TxOut] -> [TxOut]
    getScriptOutputs [] acc = acc
    getScriptOutputs (o@(TxOut (Address (ScriptCredential s) _) _ _) : tl) acc
      | s == scrHash              = getScriptOutputs tl (o : acc)
      | otherwise                 = getScriptOutputs tl acc
    getScriptOutputs (_ : tl) acc = getScriptOutputs tl acc

    -- use scrHash instead of ownHash
    scrHash :: ValidatorHash
    scrHash =
      case ownInput of
       TxOut (Address (ScriptCredential s) _) _ _ -> s
       _                                          -> error ()

    inVal, outVal :: Value
    !inVal  = txOutValue ownInput
    !outVal = txOutValue ownOutput

    oldNFT, newNFT, fees :: Integer
    !oldNFT       = assetClassValueOf inVal  nftAC
    !newNFT       = assetClassValueOf outVal nftAC
    !fees         = gpFee gp

    nftAC, counterAC, votesAC, lovelace :: AssetClass
    !nftAC     = gpNFT     gp
    !lovelace  = AssetClass (adaSymbol, adaToken)

    requests :: ResultAcc
    requests =
      let in_l = txInfoInputs $ ctxInfo in
        requests' (ResultAcc oldVotes oldCounter) in_l

    requests' :: ResultAcc -> [TxInInfo] -> ResultAcc
    requests' acc [] = acc
    requests' acc@(ResultAcc vote counter) ((TxInInfo tref ot) : tl) =
      let !v = txOutValue ot
          !c = assetClassValueOf v counterAC
      in
        if (assetClassValueOf v lovelace >= fees) &&
           (tref /= ownRef)                       &&
           (c >= 1)                               &&
           (c <= 100)
        then requests' (ResultAcc (vote + 1) (counter + c)) tl
        else requests' acc tl

    validCounters :: Bool
    validCounters =
      if | oldVotes < voteCount  ->                       -- Is voting still in progress?
           (newVotes <= voteCount)                     && -- Is the new number of votes <= the maximal number of votes?
           (requests == ResultAcc newVotes newCounter)

         | oldVotes == voteCount ->                       -- Is voting finished, but our "secret" number has not yet been added?
             (newCounter == finalCounter)              && -- Has the final result been calculated correctly?
             (newVotes   == 1 + voteCount)                -- Have the new votes been calculated correctly?

         | otherwise             ->                       -- Is voting finished, and our "secret" number has been added?
             (newCounter == oldCounter)                && -- Has the final counter value been kept?
             (newVotes   == oldVotes)                     -- Has the number of votes been kept?


    ownRef :: TxOutRef
    ownRef = case scriptContextPurpose ctx of
               Spending ref -> ref
               _            -> error ()

data GoldNaming
instance Scripts.ValidatorTypes GoldNaming where
    type instance DatumType GoldNaming = Integer
    type instance RedeemerType GoldNaming = BuiltinData

typedGoldValidator :: GoldParams -> Scripts.TypedValidator GoldNaming
typedGoldValidator gp = Scripts.mkTypedValidator @GoldNaming
    ($$(PlutusTx.compile [|| mkGoldValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode expectedDatumHash
        `PlutusTx.applyCode` PlutusTx.liftCode gp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @BuiltinData

goldValidator :: GoldParams -> Validator
goldValidator = Scripts.validatorScript . typedGoldValidator

goldScript :: GoldParams -> Plutus.Script
goldScript = Ledger.unValidatorScript . goldValidator

goldScriptAsShortBs :: GoldParams -> SBS.ShortByteString
goldScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . goldScript

apiGoldScript :: GoldParams -> PlutusScript PlutusScriptV1
apiGoldScript = PlutusScriptSerialised . goldScriptAsShortBs

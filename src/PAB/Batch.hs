-- I am a state machine that processes batches of data

import Plutus.Contract.Request

numTxOut :: Int
numTxOut = len $ utxosAt goldAddress

import Plutus.Contract.StateMachine

getScriptUTxOs

runInitialiseWith  

getThreadToken

data Batch = Batch
    { bFirst          :: !PubKeyHash
    , bSecond         :: !PubKeyHash
    , bStake          :: !Integer
    , bPlayDeadline   :: !Slot
    , bRevealDeadline :: !Slot
    , bToken          :: !AssetClass
    , bMaxInputs      :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

data BatchDatum = BatchDatum ByteString (Maybe Int)
deriving Show

instance Eq BatchDatum where
    {-# INLINABLE (==) #-}
    BatchDatum bs mc == BatchDatum bs' mc' = (bs == bs') && (mc == mc')

maxInputs 

{-# INLINABLE bFull #-}
full BatchDatum bs mc == mc >= bMaxInputs batch

batchDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe BatchDatum
batchDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromData d

transition :: Batch -> State BatchDatum -> BatchRedeemer -> Maybe (TxConstraints Void Void, State BatchDatum)
transition batch s r = case (stateValue s, stateData s, r) of
    (v, BatchDatum bs Nothing, Play c)
        | lovelaces v == gStake batch         -> Just ( Constraints.mustBeSignedBy (gSecond batch)                    <>
                                                       Constraints.mustValidateIn (to $ gPlayDeadline batch)
                                                     , State (BatchDatum bs $ Just c) (lovelaceValueOf $ 2 * gStake batch)
                                                     )
    (v, BatchDatum _ (Just _), Reveal _)
        | lovelaces v == (2 * gStake batch)   -> Just ( Constraints.mustBeSignedBy (gFirst batch)                     <>
                                                       Constraints.mustValidateIn (to $ gRevealDeadline batch)       <>
                                                       Constraints.mustPayToPubKey (gFirst batch) token
                                                     , State Finished mempty
                                                     )
    (v, BatchDatum _ Nothing, ClaimFirst)
        | lovelaces v == gStake batch         -> Just ( Constraints.mustBeSignedBy (gFirst batch)                     <>
                                                       Constraints.mustValidateIn (from $ 1 + gPlayDeadline batch)   <>
                                                       Constraints.mustPayToPubKey (gFirst batch) token
                                                     , State Finished mempty
                                                     )
    (v, BatchDatum _ (Just _), ClaimSecond)
        | lovelaces v == (2 * gStake batch)   -> Just ( Constraints.mustBeSignedBy (gSecond batch)                    <>
                                                       Constraints.mustValidateIn (from $ 1 + gRevealDeadline batch) <>
                                                       Constraints.mustPayToPubKey (gFirst batch) token
                                                     , State Finished mempty
                                                     )
    _                                        -> Nothing
  where
    token :: Value
    token = assetClassValue (gToken batch) 1

batchStateMachine :: Batch -> ByteString -> ByteString -> StateMachine BatchDatum BatchRedeemer
batchStateMachine batch bsZero' bsOne' = StateMachine
    { smTransition  = transition batch
    , smFinal       = final
    , smCheck       = check bsZero' bsOne'
    , smThreadToken = Just $ gToken batch
    }

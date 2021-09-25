import Cardano.Api                           hiding (TxId)
import Data.String                           (IsString (..))
import Ledger.Value                          (AssetClass (..))
import Prelude
import System.Environment                    (getArgs)

import PAB.RequestPolicies
import PAB.RequestGoldScript

main :: IO ()
main = do
    [nftSymbol, otherSymbol, seed', nameCount', voteCount', fee'] <- getArgs
    let seed      = read seed'
        nft       = AssetClass (fromString nftSymbol,   nftTokenName)
        counter   = AssetClass (fromString otherSymbol, counterTokenName)
        votes     = AssetClass (fromString otherSymbol, votesTokenName)
        nameCount = read nameCount'
        voteCount = read voteCount'
        fee       = read fee'
        gp          = GoldParams
            { gpSeed      = seed
            , gpNFT       = nft
            , gpCounter   = counter
            , gpVotes     = votes
            , gpNameCount = nameCount
            , gpVoteCount = voteCount
            , gpFee       = fee
            }
        goldFile = "scripts/gold.plutus"
        requestFile = "scripts/gold-request.plutus"
    print gp

    lobsterResult <- writeFileTextEnvelope lobsterFile Nothing $ apiLobsterScript gp
    case goldResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote lobster script to file " ++ goldFile

    requestResult <- writeFileTextEnvelope requestFile Nothing $ apiRequestScript gp
    case requestResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote request script to file " ++ requestFile

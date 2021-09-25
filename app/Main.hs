import Cardano.Api                           hiding (TxId)
import Data.String                           (IsString (..))
import Ledger.Value                          (AssetClass (..))
import Prelude
import System.Environment                    (getArgs)

import PAB.RequestPolicies
import PAB.RequestGoldScript
import PAB.CreateRequest

main :: IO ()
main = do
    [nftSymbol, otherSymbol, fee'] <- getArgs
    let nft       = AssetClass (fromString nftSymbol, nftTokenName)
        fee       = read fee'
        gp          = GoldParams
            { gpSeed      = seed
            , gpNFT       = nft
            , gpFee       = fee
            }
        goldFile = "scripts/gold.plutus"
        requestFile = "scripts/gold-request.plutus"
    print gp

    goldResult <- writeFileTextEnvelope goldFile Nothing $ apiGoldScript gp
    case goldResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote gold script to file " ++ goldFile

    requestResult <- writeFileTextEnvelope requestFile Nothing $ apiRequestScript gp
    case requestResult of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote request script to file " ++ requestFile

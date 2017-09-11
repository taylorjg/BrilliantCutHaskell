module Main where

import BrilliantCut
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
    let inputJson = "src/main/input.json"
    bs <- BS.readFile inputJson
    case (largestProfitByteString  bs) of
        Just answer -> putStrLn $ "Largest profit: " ++ show answer
        _ -> putStrLn $ "Error parsing " ++ inputJson

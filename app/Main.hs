module Main where

import BrilliantCut
import qualified Data.ByteString.Lazy.Char8 as C8

main :: IO ()
main = do
    let inputJson = "app/input.json"
    bs <- C8.readFile inputJson
    case (largestProfitByteString bs) of
        Just answer -> putStrLn $ "Largest profit: " ++ show answer
        _ -> putStrLn $ "Error parsing " ++ inputJson

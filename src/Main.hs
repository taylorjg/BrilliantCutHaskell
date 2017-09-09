{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HM
    
data Cut = Cut {
    size :: Int,
    value :: Int
} deriving (Generic, Show)

data Gem = Gem {
    cuts :: [Cut],
    rawChunks :: [Int]
} deriving (Generic, Show)

data Input = Input {
    gems :: [Gem]
} deriving Show

instance FromJSON Cut

instance FromJSON Gem

instance FromJSON Input where
    parseJSON = withObject "Input" $ \obj ->
        let
            vs = HM.elems obj
            gemParser = parseJSON :: Value -> Parser Gem
            gemsParser = mapM gemParser vs
        in
            Input <$> gemsParser

generateCombinationsOfCuts :: Int -> [Cut] -> [Cut] -> [[Cut]]
generateCombinationsOfCuts chunkSize availableCuts actualCuts =
    do
        cut <- availableCuts
        let chunkSize' = chunkSize - size cut
        case chunkSize' of
            _ | chunkSize' > 0 ->
                let
                    actualCuts' = cut:actualCuts
                    rest = generateCombinationsOfCuts chunkSize' availableCuts actualCuts'
                in actualCuts':rest
            _ -> []

calculateProfitForCombinationOfCuts :: Int -> [Cut] -> Int
calculateProfitForCombinationOfCuts rawChunk cuts =
    profit
    where
        vsum = sum $ map value cuts
        ssum = sum $ map size cuts
        waste = rawChunk - ssum
        profit = vsum - waste
        
calculateMaxProfitForRawChunk :: [Cut] -> Int -> Int
calculateMaxProfitForRawChunk availableCuts rawChunk =
    v3
    where
        v1 = generateCombinationsOfCuts rawChunk availableCuts []
        v2 = map (calculateProfitForCombinationOfCuts rawChunk) v1
        v3 = maximum v2

calculateMaxProfitsForRawChunks :: Gem -> [Int]
calculateMaxProfitsForRawChunks gem =
    map (calculateMaxProfitForRawChunk (cuts gem)) (rawChunks gem)

largestProfit :: Input -> Int
largestProfit = sum . map sum . map calculateMaxProfitsForRawChunks . gems

main :: IO ()
main = do
    let inputJson = "/Users/jontaylor/HomeProjects/BrilliantCutHaskell/src/input.json"
    bs <- BS.readFile inputJson
    let maybeInput = decode bs :: Maybe Input
    case maybeInput of
        Just input ->
            let answer = largestProfit input
            in putStrLn $ "Largest profit: " ++ show answer
        _ -> putStrLn "Error reading input.json"

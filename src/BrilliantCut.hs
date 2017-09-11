{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module BrilliantCut (
    largestProfitByteString,
    largestProfitGems
    ) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as C8
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
    maximum . map (calculateProfitForCombinationOfCuts rawChunk)
        $ generateCombinationsOfCuts rawChunk availableCuts []

calculateMaxProfitsForRawChunks :: Gem -> [Int]
calculateMaxProfitsForRawChunks gem =
    map (calculateMaxProfitForRawChunk (cuts gem)) (rawChunks gem)

largestProfitGems :: [Gem] -> Int
largestProfitGems = sum . map sum . map calculateMaxProfitsForRawChunks

largestProfitByteString :: C8.ByteString -> Maybe Int
largestProfitByteString bs =
    (largestProfitGems . gems) `fmap` maybeInput
    where maybeInput = decode bs :: Maybe Input

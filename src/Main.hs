{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS
    
data Cut = Cut {
    size :: Int,
    value :: Int
} deriving (Generic, Show)

instance FromJSON Cut

data Gem = Gem {
    cuts :: [Cut],
    rawChunks :: [Int]
} deriving (Generic, Show)

instance FromJSON Gem

main :: IO ()
main = do
    let diamondJson = "/Users/jontaylor/HomeProjects/BrilliantCutHaskell/src/diamond.json"
    getJSON <- BS.readFile diamondJson
    let maybeDiamond = decode getJSON :: Maybe Gem
    putStrLn $ show maybeDiamond

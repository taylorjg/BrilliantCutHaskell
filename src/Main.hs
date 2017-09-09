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

main :: IO ()
main = do
    let inputJson = "/Users/jontaylor/HomeProjects/BrilliantCutHaskell/src/input.json"
    bs <- BS.readFile inputJson
    let maybeInput = decode bs :: Maybe Input
    putStrLn $ show maybeInput

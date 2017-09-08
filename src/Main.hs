{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS
    
data Person = Person {
    name :: String,
    age  :: Int
} deriving (Generic, Show)    

instance ToJSON Person where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Person

main :: IO ()
main = BS.putStrLn $ encode (Person {name = "Joe", age = 12})

{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics
import Data.Text
import Data.Maybe
import Data.ByteString.Lazy as L

data Person = 
    Person {name :: Text, age :: Int} 
        deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

readJ = do 
    p <- L.readFile "test.json"
    let j = decode p :: Maybe Person
        a = fromJust j
        in return $ (name a, age a)

readAJ = do
    p <- L.readFile "testA.json"
    return p

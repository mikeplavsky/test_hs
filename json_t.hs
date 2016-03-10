{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics
import Data.Text
import Data.ByteString.Lazy as L

data Person = 
    Person {name :: Text, age :: Int} 
        deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

readJ = do 
    p <- L.readFile "test.json"
    let j1 = decode p :: Maybe Person
        in return j1

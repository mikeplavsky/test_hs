{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.Wreq
import Control.Lens
import Data.Aeson (Array, FromJSON, ToJSON, decode)
import Data.Text
import GHC.Generics
import Data.Maybe
import qualified Data.ByteString as S

pt_url = "https://www.pivotaltracker.com/services/v5/projects"
pt_stories = pt_url ++ "/1367594/iterations?offset=" 

data Story = Story {
      name :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Story

data Iteration = Iteration {
      start :: Text,
      finish :: Text,
      stories :: [Story]
  } deriving (Eq, Show, Generic)

instance FromJSON Iteration

get_raw tk offset =
    let opts = defaults & header "X-TrackerToken" .~ [tk]
    in do getWith opts $ pt_stories ++ (show offset)

get_iterations s = 
    let d = s ^. responseBody
    in decode d :: Maybe [Iteration]

get_all :: S.ByteString -> Int -> [[Iteration]] -> IO [[Iteration]]
get_all tk offset its = do

        s <- get_raw tk offset 
        let i = fromJust $ get_iterations s 

        putStrLn $ show offset

        if i == []
          then return its 
          else get_all tk (offset + 10) (i:its)

get_project :: S.ByteString -> IO [[Iteration]]
get_project tk = get_all tk 0 []


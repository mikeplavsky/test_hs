{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.Wreq
import Control.Lens
import Data.Aeson (Array, FromJSON, ToJSON, decode)
import Data.Text
import GHC.Generics

pt_url = "https://www.pivotaltracker.com/services/v5/projects"
pt_stories = pt_url ++ "/1367594/iterations?offset=0" 

data Story = Story {
      name :: Text
  } deriving (Show, Generic)

instance FromJSON Story

data Iteration = Iteration {
      stories :: [Story]
  } deriving (Show, Generic)

instance FromJSON Iteration

get_raw_stories tk =
    let opts = defaults & header "X-TrackerToken" .~ [tk]
    in do getWith opts pt_stories

get_stories s = 
    let d = s ^. responseBody
    in decode d :: Maybe [Iteration]

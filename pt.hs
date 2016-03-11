{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.Wreq
import Control.Lens
import Data.Aeson (Array, FromJSON, ToJSON)
import Data.Text
import GHC.Generics

projects = "https://www.pivotaltracker.com/services/v5/projects"
type Resp = Response (Array)

get_projects1 tk =

    let opts = defaults & header "X-TrackerToken" .~ [tk]
    in 
       let d = getWith opts projects
           d1 = asJSON =<< d :: IO Resp
       in d1
        
data Project = 
    Project {name :: Text} 
        deriving (Show, Generic)

instance FromJSON Project
instance ToJSON Project

get_projects tk =

    let opts = defaults & header "X-TrackerToken" .~ [tk]
    in do getWith opts projects

get_data r = r ^. responseBody



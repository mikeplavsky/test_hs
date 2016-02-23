{-# LANGUAGE OverloadedStrings #-}
import Network.Wreq
import Control.Lens
import Data.Aeson (Array)

projects = "https://www.pivotaltracker.com/services/v5/projects"
type Resp = Response (Array)

get_projects1 tk =

    let opts = defaults & header "X-TrackerToken" .~ [tk]
    in 
       let d = getWith opts projects
           d1 = asJSON =<< d :: IO Resp
       in d1
        
get_projects tk =

    let opts = defaults & header "X-TrackerToken" .~ [tk]
    in do getWith opts projects



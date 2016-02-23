{-# LANGUAGE OverloadedStrings #-}
import Network.Wreq
import Control.Lens

projects = "https://www.pivotaltracker.com/services/v5/projects"

get_projects tk =

    let opts = defaults & header "X-TrackerToken" .~ [tk]
    in do getWith opts projects



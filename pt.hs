import Network.Wreq
import Control.Lens

projects = "https://www.pivotaltracker.com/services/v5/projects"

get_projects = do
    r <- get projects
    return  r

main = get_projects

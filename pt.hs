import Network.Wreq
import Control.Lens

projects = "https://www.pivotaltracker.com/services/v5/projects"

get_projects tk = do
    r <- get projects
    return  r

token = "ABCD"

main = do 
    r <-(get_projects token)
    putStrLn $ show $ r ^. responseStatus

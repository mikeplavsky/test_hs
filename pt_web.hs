{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.Wreq
import Control.Lens
import Data.Aeson (Array, FromJSON, ToJSON, decode)
import Data.Text
import GHC.Generics
import Data.Maybe
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified GHC.List as L
import qualified Control.Monad as M
import Data.DateTime
import System.Environment

pt_tk = getEnv "PT_TOKEN"

pt_url = "https://www.pivotaltracker.com/services/v5/projects"

pt_stories = do 
    pt_project_id <- getEnv "PT_PROJECT_ID"
    return $ pt_url ++ "/" ++ pt_project_id ++ "/iterations?offset=" 

pt_start_date = do 
    d <- getEnv "PT_START_DATE"
    return $ get_time $ pack d
 
pt_release_name = getEnv "PT_RELEASE_NAME"

main = print_burndown

print_burndown = do

    rn <- pt_release_name
    sd <- pt_start_date

    (t,b) <- get_burndown sd rn 

    putStrLn $ "Total: " ++ show t
    mapM_ (\(x,y) -> 
        putStrLn $ show x ++ "\t" ++ show y) b

get_burndown start_date release_name = do

    tk <- pt_tk
    its <- get_all_iterations $ C.pack tk    

    let finish_d = find_finish_date release_name its
        f_its = release_iterations its start_date finish_d 

    return $ (iterations_total f_its, done_iterations f_its)

data Story = Story {
    name :: Text,
    story_type :: Text,
    current_state :: Text,
    estimate :: Maybe Int
} deriving (Eq, Show, Generic)

instance FromJSON Story

data Iteration = Iteration {
    start :: Text,
    finish :: Text,
    stories :: [Story]
} deriving (Eq, Show, Generic)

instance FromJSON Iteration

get_raw tk offset = do

    let opts = defaults & header "X-TrackerToken" .~ [tk]

    stories <- pt_stories      
    getWith opts $ stories ++ (show offset)

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

get_all_iterations tk = do 

    its <- get_project tk 

    let s = L.reverse its
    return $ M.join s 

get_time t = 
    fromJust $ parseDateTime "%Y-%m-%dT%H:%M:%SZ" $ unpack t

stories_total sts = 
    L.foldl (\acc s -> acc + (fromMaybe 0 $ estimate s)) 0 sts

stories_done sts = 
    L.foldl (\acc s -> 
        if (unpack $ current_state s) == "accepted" 
        then acc + (fromMaybe 0 $ estimate s)
        else acc) 0 sts

iterations_total its =
    L.foldl (\acc i -> acc + (stories_total $ stories i)) 0 its

release_iterations its start_t finish_t =
    L.filter (\i -> 
        ((get_time $ start i) >= start_t) && 
        ((get_time $ finish i) <= finish_t)) its

find_release release_n sts = 
    L.filter (\s -> 
        (story_type s == "release") && 
        ((unpack $ name s)== release_n)) sts

find_finish_date release_n its = 
    get_time $ finish $ L.head $ L.filter (\i -> 
        (L.length $ find_release release_n $ stories i) /= 0) its 

done_iterations its = 
    L.map (\i -> 
        ((finish i),(stories_done (stories i)))) its



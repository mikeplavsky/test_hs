{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.Wreq
import Control.Lens
import Data.Aeson (Array, FromJSON, ToJSON, decode)
import Data.Text
import GHC.Generics
import Data.Maybe
import qualified Data.ByteString as S
import qualified GHC.List as L
import qualified Control.Monad as M
import Data.DateTime

pt_url = "https://www.pivotaltracker.com/services/v5/projects"
pt_stories = pt_url ++ "/1367594/iterations?offset=" 

start_date = get_time $ pack "2015-10-01T21:00:00Z" 
release_name = "ODME RTM 1.9"

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



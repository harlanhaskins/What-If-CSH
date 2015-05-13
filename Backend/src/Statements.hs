module Statements where
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad hiding (forM_, mapM_, forM, mapM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import Data.Foldable
import Data.Aeson
import Data.Time.Clock
import Data.Time.Calendar
import GHC.Generics
import Data.Proxy
import Debug.Trace

import qualified Hasql as H
import qualified Hasql.Postgres as HP
import qualified Data.Text as T

add :: T.Text -> T.Text -> H.Stmt HP.Postgres
add user description = [H.stmt|
    insert into suggestion (submitter, description, created_at, active) values ($user, $description, current_timestamp, true)
    returning id, description, created_at, active, submitter, 0 as score, 0 as vote
|]

get :: T.Text -> H.Stmt HP.Postgres
get user = [H.stmt|
    select suggestion.*,
           coalesce(sum(vote.vote), 0) as score,
           coalesce((select vote from vote where member = $user and vote.suggestion_id = suggestion.id), 0) as vote
    from suggestion left join vote on vote.suggestion_id = suggestion.id
    where suggestion.active = true
    group by suggestion.id
    order by suggestion.created_at
    desc limit 30
|]

remove :: Int -> T.Text -> H.Stmt HP.Postgres
remove id submitter = [H.stmt|
    update suggestion set active = False where id = $id and submitter = $submitter
|]

vote :: Int -> T.Text -> Int -> H.Stmt HP.Postgres
vote id user voteType = [H.stmt|
    insert into vote values ($id, $voteType, $user)
|]

unvote :: Int -> T.Text -> H.Stmt HP.Postgres
unvote id user = [H.stmt|
    delete from vote where suggestion_id = $id and member = $user
|]

example pw = do
    let postgresSettings = HP.ParamSettings "postgres.csh.rit.edu" 5432 "harlan_whatifcsh" pw "harlan_whatifcsh"

    poolSettings <- maybe (fail "Improper session settings") return $
                    H.poolSettings 6 30

    pool :: H.Pool HP.Postgres
         <- H.acquirePool postgresSettings poolSettings

    result <- H.session pool $ do
        row <- H.tx (Just (H.Serializable, (Just True))) $ H.singleEx $ add "harlan" "test with hasql"
        liftIO $ (putStrLn . showSubmission) row
        do
            submissions <- H.tx Nothing $ H.listEx (get "harlan")
            forM_ submissions $ \s -> do
                liftIO $ (putStrLn . showSubmission) s
    liftIO $ printResult result

    H.releasePool pool


printResult (Left c)  = print c
printResult (Right _) = putStrLn "Success."

showSubmission :: (Int, T.Text, UTCTime, Bool, T.Text, Int, Int) -> String
showSubmission (id, content, timestamp, active, submitter, score, votes) = 
    "ID: " ++ show id ++ ", Content: " ++ (T.unpack content) ++ ", Submitted by: " ++ (T.unpack submitter) ++ ", Score: " ++ show score

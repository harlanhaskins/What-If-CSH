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
import Data.ByteString (ByteString)

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
           coalesce((
            select vote from vote
                where member = $user
                  and vote.suggestion_id = suggestion.id)
            , 0) as vote
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

deleteTable = [H.stmt|
    drop table if exists suggestion cascade
|]

createTable = [H.stmt|
    create table suggestion (
        id bigserial primary key,
        description varchar(140) not null,
        created_at timestamptz default current_timestamp,
        active boolean not null,
        submitter varchar(100) not null
    )
|]

createVoteTable = [H.stmt|
    create table vote (
        suggestion_id bigint not null references suggestion (id),
        vote integer not null CHECK (vote = -1 or vote = 1),
        member varchar(100) default 'nobody' not null
    )
|]

createIndices :: H.Session HP.Postgres IO ()
createIndices = do
    mapM_ queryUnit [createSuggestionIndex,
                     createSubmitterIndex,
                     createVoteSuggestionIndex,
                     createVoteMemberIndex]

createVoteSuggestionIndex = [H.stmt|
    create index "vote_suggestion_id_index" on vote (suggestion_id)
|]

createVoteMemberIndex = [H.stmt|
    create index "vote_member_index" on vote (member)
|]

createSubmitterIndex = [H.stmt|
    create index "suggestion_submitter_index" on suggestion (submitter)
|]

createSuggestionIndex = [H.stmt|
    create index "suggestion_id_index" on suggestion (id)
|]

postgresSettings pw = HP.ParamSettings "postgres.csh.rit.edu" 5432 "harlan_whatifcsh" pw "harlan_whatifcsh"

connectPool :: ByteString -> IO (H.Pool HP.Postgres)
connectPool pw = do
    poolSettings <- maybe (fail "Improper session settings") return $
                    H.poolSettings 6 30

    H.acquirePool (postgresSettings pw) poolSettings

example pw = do

    pool <- connectPool pw

    result <- H.session pool $ do
        liftIO $ putStrLn "\nAdding now row.."
        row <- querySingle $ add "harlan" "test with hasql"
        printSubmission row

        liftIO $ putStrLn "\nRemoving existing votes on submission."
        queryUnit $ unvote (idFromSubmission row) "harlan"

        liftIO $ putStrLn "\nUpvoting submission."
        queryUnit $ vote (idFromSubmission row) "harlan" 1

        liftIO $ putStrLn "\nRetrieving latest posts..."
        submissions <- queryList $ get "harlan"
        mapM_ printSubmission submissions

        liftIO $ putStrLn "\nDeleting post from earlier."
        queryUnit $ remove (idFromSubmission row) "harlan"
    printResult result

    H.releasePool pool

queryUnit   s = H.tx (Just (H.Serializable, (Just True))) $ H.unitEx s
querySingle s = H.tx (Just (H.Serializable, (Just True))) $ H.singleEx s
queryList   s = H.tx Nothing                              $ H.listEx s

printResult (Left c)  = print c
printResult (Right _) = putStrLn "Success."

idFromSubmission (id, _, _, _, _, _, _) = id

printSubmission = liftIO . putStrLn . showSubmission

showSubmission :: (Int, T.Text, UTCTime, Bool, T.Text, Int, Int) -> String
showSubmission (id, content, timestamp, active, submitter, score, votes) = "ID: " ++ show id
                                                                        ++ ", Content: " ++ (T.unpack content)
                                                                        ++ ", Submitted by: " ++ (T.unpack submitter)
                                                                        ++ ", Time: " ++ show timestamp
                                                                        ++ ", Score: " ++ show score
                                                                        ++ ", Vote: " ++ show votes


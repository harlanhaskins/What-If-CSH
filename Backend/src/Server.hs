{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import Data.Time.Clock
import Data.Time.Calendar
import GHC.Generics
import Data.Proxy
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Time
import qualified Control.Monad.Trans.Either as E
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.Environment
import qualified Data.ByteString as B
import qualified Statements as S
import Data.String
import qualified Data.Text as T

strip  = T.unpack . T.strip . T.pack
lstrip = T.unpack . T.stripStart . T.pack
rstrip = T.unpack . T.stripEnd . T.pack

data Suggestion = Suggestion
  { id :: Integer
  , content :: String
  , timestamp :: UTCTime
  , active :: Bool
  , submitter :: String
  , score :: Int
  , vote :: Maybe Int
  } deriving (Generic, ToJSON)

data Status = Status
  { user :: String
  , suggestions :: [Suggestion]
  } deriving (Generic, ToJSON)

data Submission = Submission
  { description :: String
  } deriving Generic

instance FromJSON Submission
    where parseJSON (Object v) = do
                        desc <- (v .: "description")
                        case (validatedDescription desc) of
                            (Just d) -> Submission <$> return d
                            Nothing -> empty

maxLength = 140

validatedDescription = validated' . strip
    where validated' s
            | (length s > maxLength) = Just (take maxLength s)
            | (length s > 0)         = Just s
            | otherwise              = Nothing

type SuggestionAPI = "suggestions" :> ReqBody Submission :> Header "X-WEBAUTH-USER" String :> Post Suggestion
                :<|> "suggestions" :> Header "X-WEBAUTH-USER" String :> Get Status
                :<|> "suggestions" :> Capture "id" Integer :> Header "X-WEBAUTH-USER" String :> Delete
                :<|> "suggestions" :> Capture "id" Integer :> Header "X-WEBAUTH-USER" String :> "vote" :> Capture "type" String :> Put ()
                :<|> "suggestions" :> Capture "id" Integer :> Header "X-WEBAUTH-USER" String :> "vote" :> Delete

server :: Database.PostgreSQL.Simple.Connection -> Server SuggestionAPI
server conn = add :<|> get :<|> remove :<|> vote :<|> unvote
    where add submission user = liftIO $ querySingle $ add user (description submission)
          get Nothing = E.left (404, "Invalid user.")
          get (Just user) = liftIO $ query conn getQuery (Only user) >>= return . (Status user)
          remove id user = liftIO $ execute conn "update suggestion set active = FALSE where id = ? and submitter = ?" [toField id, toField user] >> return ()
          unvote id user = liftIO $ execute conn "delete from vote where suggestion_id = ? and member = ?" [toField id, toField user] >> return ()
          vote id user voteType = vote' id user (intFromType voteType)
          vote' id user voteType = liftIO $ execute conn ("delete from vote where suggestion_id = ? and member = ?; insert into vote values (?, ?, ?)") [toField id, toField user, toField id, toField voteType, toField user] >> return ()
          intFromType :: String -> Integer
          intFromType voteType = case voteType of
                                    "up"   ->  1
                                    "down" -> -1
                                    _      ->  0

-- ranking algorithm from http://amix.dk/blog/post/19588
getQuery = "select suggestion.*, coalesce(sum(vote.vote), 0) as score, coalesce((select vote from vote where member = ? and vote.suggestion_id = suggestion.id), 0) as vote from suggestion left join vote on vote.suggestion_id = suggestion.id where suggestion.active = true group by suggestion.id order by suggestion.created_at desc limit 30"

suggestionAPI :: Proxy SuggestionAPI
suggestionAPI = Proxy

resourcePolicy = CorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = "PUT":"DELETE":simpleMethods
    , corsRequestHeaders = simpleHeaders
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

main = do
    [pw] <- getArgs
    (connectPostgreSQL . fromString . concat) ["host=postgres.csh.rit.edu user=harlan_whatifcsh dbname=harlan_whatifcsh password=", pw]
        >>= run 5777
          . (cors . const . Just) resourcePolicy
          . serve suggestionAPI
          . server


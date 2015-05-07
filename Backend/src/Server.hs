{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Time
import Data.Time.Clock
import Data.Time.Calendar
import GHC.Generics
import qualified Control.Monad.Trans.Either as E
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.Environment
import qualified Data.ByteString as B
import Data.String
import qualified Data.Text as T

strip  = T.unpack . T.strip . T.pack
lstrip = T.unpack . T.stripStart . T.pack
rstrip = T.unpack . T.stripEnd . T.pack

data Suggestion = Suggestion
  { id :: Integer
  , description :: String
  , timestamp :: UTCTime
  , active :: Bool
  , submitter :: String
  } deriving Generic

data Status = Status
  { user :: String
  , suggestions :: [Suggestion]
  } deriving Generic
instance ToJSON Status

instance FromJSON Suggestion
    where parseJSON (Object v) = do
                        desc <- (v .: "description")
                        case (validatedDescription desc) of
                            (Just d) -> Suggestion
                                    <$> return 0
                                    <*> return d
                                    <*> return defaultTime
                                    <*> return True
                                    <*> return ""
                            Nothing -> empty
instance ToJSON Suggestion
    where toJSON (Suggestion id desc time _ submitter) = object [ "description" .= desc
                                                      , "id" .= id
                                                      , "score" .= (0 :: Integer)
                                                      , "timestamp" .= time
                                                      , "submitter" .= submitter]

defaultTime = (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))
maxLength = 140

validatedDescription = validated' . strip
    where validated' s
            | (length s > maxLength) = Just (take maxLength s)
            | (length s > 0)   = Just s
            | otherwise        = Nothing

-- PostgreSQL instances
instance FromRow Suggestion where
  fromRow = Suggestion <$> field <*> field <*> field <*> field <*> field

instance ToRow Suggestion where
  toRow s = [toField (description s)]

type SuggestionAPI = "suggestions" :> ReqBody Suggestion :> Header "X-WEBAUTH-USER" String :> Post Suggestion
                :<|> "suggestions" :> Header "X-WEBAUTH-USER" String :> Get Status
                :<|> "suggestions" :> Capture "id" Integer :> Header "X-WEBAUTH-USER" String :> Delete
                :<|> "suggestions" :> Capture "id" Integer :> Header "X-WEBAUTH-USER" String :> "vote" :> Capture "type" String :> Put ()

server :: Database.PostgreSQL.Simple.Connection -> Server SuggestionAPI
server conn = add :<|> get :<|> remove :<|> vote
    where add suggestion user = liftIO $ execute conn "insert into suggestion (submitter, description, created_at, active) values (?, ?, current_timestamp, true)" ((toField user):(toRow suggestion)) >> return suggestion
          get Nothing = E.left (404, "Invalid user.")
          get (Just user) = liftIO $ query_ conn "select * from suggestion where active = TRUE order by created_at desc limit 30" >>= return . (Status user)
          remove id user      = liftIO $ execute conn "update suggestion set active = FALSE where id = ? and submitter = ?" [toField id, toField user] >> return ()
          vote id user voteType 
              | elem voteType ["upvote", "downvote"] = liftIO $ execute conn ("delete from vote where suggestion_id = ? and member_uuid = ?; insert into vote values (?, ?, ?)") [toField id, toField user, toField id, toField voteType, toField user] >> return ()               
              | otherwise = E.left (404, "Not Found")


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


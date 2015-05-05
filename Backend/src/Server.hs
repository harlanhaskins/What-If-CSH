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
import GHC.Generics
import Network.Wai.Handler.Warp (run)
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
  , upvotes :: Integer
  , downvotes :: Integer
  } deriving Generic

instance FromJSON Suggestion
    where parseJSON (Object v) = do
                        desc <- (v .: "description")
                        case (validatedDescription desc) of
                            (Just d) -> Suggestion
                                    <$> return 0
                                    <*> return d
                                    <*> return 0 -- upvotes start at 0
                                    <*> return 0 -- downvotes start at 0
                            Nothing -> empty
instance ToJSON Suggestion

maxLength = 140

validatedDescription = validated' . strip
    where validated' s
            | (length s > maxLength) = Just (take maxLength s)
            | (length s > 0)   = Just s
            | otherwise        = Nothing

-- PostgreSQL instances
instance FromRow Suggestion where
  fromRow = Suggestion <$> field <*> field <*> field <*> field

instance ToRow Suggestion where
  toRow s = [toField (description s),
             toField (upvotes s),
             toField (downvotes s)]

type SuggestionAPI = "suggestions" :> ReqBody Suggestion :> Post Suggestion
                :<|> "suggestions" :> Get [Suggestion]
                :<|> "suggestions" :> Capture "id" Integer :> Delete

server :: Connection -> Server SuggestionAPI
server conn = add :<|> get :<|> remove
    where add suggestion = liftIO $ execute conn "insert into suggestions (description, upvotes, downvotes) values (?, ?, ?)" suggestion >> return suggestion
          get            = liftIO $ query_ conn "select * from suggestions order by id desc limit 30"
          remove id      = liftIO $ execute conn "delete from suggestions where id = ?" (Only id) >> return ()

suggestionAPI :: Proxy SuggestionAPI
suggestionAPI = Proxy

resourcePolicy = CorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = simpleMethods
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


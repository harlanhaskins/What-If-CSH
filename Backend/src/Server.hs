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

data Suggestion = Suggestion
  { description :: String
  } deriving Generic

instance FromJSON Suggestion
instance ToJSON Suggestion

-- PostgreSQL instances
instance FromRow Suggestion where
  fromRow = Suggestion <$> field

instance ToRow Suggestion where
  toRow s = [toField (description s)]

type SuggestionAPI = "suggestions" :> ReqBody Suggestion :> Post Suggestion
                :<|> "suggestions" :> Get [Suggestion]

server :: Connection -> Server SuggestionAPI
server conn = add :<|> get
    where add suggestion = liftIO $ execute conn "insert into suggestions values (?)" suggestion >> return suggestion
          get            = liftIO $ query_ conn "select * from suggestions"

suggestionAPI :: Proxy SuggestionAPI
suggestionAPI = Proxy

main = do
    [pw] <- getArgs
    (connectPostgreSQL . fromString . concat) ["host=postgres.csh.rit.edu user=harlan_whatifcsh dbname=harlan_whatifcsh password=", pw]
        >>= run 5777
          . simpleCors
          . serve suggestionAPI
          . server


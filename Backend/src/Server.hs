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
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Servant
import System.Environment

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
               :<|> "suggestions" :> Get [Character]

server :: Connection -> Server SuggestionAPI
server conn = postCharacter
         :<|> getCharacters
         :<|> getQuotes
    where add suggestion = liftIO $ execute conn "insert into suggestions values (?)" suggestion >> return suggestion
          get            = liftIO $ query_ conn "select * from suggestions"

suggestionAPI :: Proxy SuggestionAPI
suggestionAPI = Proxy

main = connectPostgreSQL "host=localhost user=whatifcsh dbname=whatifcsh"
           >>= run 8080
             . serve suggestionAPI
             . server


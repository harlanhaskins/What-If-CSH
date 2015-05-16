import Control.Applicative
import Control.Monad.IO.Class
import Data.Time.Clock
import Data.Aeson
import GHC.Generics
import Data.Proxy
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.Environment
import Data.String
import qualified Data.ByteString.Char8 as BSC
import qualified Control.Monad.Trans.Either as E
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Statements as S
import qualified Hasql as H
import qualified Hasql.Postgres as HP

strip  = T.unpack . T.strip . T.pack
lstrip = T.unpack . T.stripStart . T.pack
rstrip = T.unpack . T.stripEnd . T.pack

data Suggestion = Suggestion
  { id :: Int
  , content :: T.Text
  , timestamp :: UTCTime
  , active :: Bool
  , submitter :: T.Text
  , score :: Int
  , vote :: Maybe Int
  } deriving (Generic, ToJSON)

data Status = Status
  { user :: T.Text
  , suggestions :: [Suggestion]
  } deriving (Generic, ToJSON)

data Submission = Submission
  { description :: T.Text
  } deriving Generic

instance FromJSON Submission
    where parseJSON (Object v) = do
                        desc <- (v .: "description")
                        case (validatedDescription desc) of
                            (Just d) -> Submission <$> (return . T.pack) d
                            Nothing -> empty

maxLength = 140

validatedDescription = validated' . strip
    where validated' s
            | (length s > maxLength) = Just (take maxLength s)
            | (length s > 0)         = Just s
            | otherwise              = Nothing

type SuggestionAPI = "suggestions" :> Header "X-WEBAUTH-USER" T.Text :> ReqBody Submission :> Post Suggestion
                :<|> "suggestions" :> Header "X-WEBAUTH-USER" T.Text :> Get Status
                :<|> "suggestions" :> Header "X-WEBAUTH-USER" T.Text :> Capture "id" Int :> Delete
                :<|> "suggestions" :> Header "X-WEBAUTH-USER" T.Text :> Capture "id" Int :> "vote" :> Capture "type" T.Text :> Put ()
                :<|> "suggestions" :> Header "X-WEBAUTH-USER" T.Text :> Capture "id" Int :> "vote" :> Delete

server :: H.Pool HP.Postgres -> Server SuggestionAPI
server pool = add :<|> get :<|> remove :<|> vote :<|> unvote
    where add Nothing _ = fail
          add (Just user) submission = runQuery $ H.session pool $ do
                tuple <- S.querySingle $ S.add user (description submission)
                return (suggestionFromTuple tuple)
          get Nothing = fail
          get (Just user) = runQuery $ H.session pool $ do
                tuples <- S.queryList $ S.get user
                return (statusFromTuples user tuples)
          remove Nothing _ = fail
          remove (Just user) id = runQuery $ H.session pool $ do
                S.queryUnit $ S.remove id user
          unvote Nothing _ = fail
          unvote (Just user) id = runQuery $ H.session pool $ do
                S.queryUnit $ S.unvote id user
          vote Nothing _ _ = fail
          vote (Just user) id voteType = runQuery $ H.session pool $ do
                S.queryUnit $ S.unvote id user
                S.queryUnit $ S.vote id user (intFromType voteType)
          intFromType :: T.Text -> Int
          intFromType voteType = case voteType of
                                    "up"   ->  1
                                    "down" -> -1
                                    _      ->  0
          runQuery = (>>= toServant)
          fail = E.left (403, "Invalid user.")

toServant (Left dbError) = E.left (500, show dbError)
toServant (Right value) = E.right value

suggestionFromTuple (id, content, timestamp, active, submitter, score, votes) = (Suggestion id content timestamp active submitter score votes)
statusFromTuples user tuples = (Status user (map suggestionFromTuple tuples))

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
    S.connectPool (BSC.pack pw)
        >>= run 5777
          . (cors . const . Just) resourcePolicy
          . serve suggestionAPI
          . server


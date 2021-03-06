module Lib where

import System.Environment
import Aws.Lambda
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Simple
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Scientific as Sci
import qualified Network.AWS.SQS as SQS
import qualified Network.AWS.Types as AWS
import Control.Monad.Trans.AWS 
  (newEnv, Credentials(Discover), runResourceT, runAWST, within, Region(Ireland), send, AWST)
import qualified Control.Lens as L

data Story = Story { storyId :: Int } deriving (Show)
instance ToJSON Story where
  toJSON (Story id) = Number $ Sci.scientific (toInteger id) 1
instance FromJSON Story where
  parseJSON (Number x) = maybe (typeMismatch "Int" $ Number x) (return . Story . Prelude.id) $ Sci.toBoundedInteger x
  parseJSON invalid = typeMismatch "Int" invalid

newtype Stories = Stories { stories :: [Story] } deriving (Show)
instance ToJSON Stories where
  toJSON (Stories stories) = Array $ V.fromList $ toJSON <$> stories
instance FromJSON Stories where
  parseJSON (Array a) = Stories <$> traverse parseJSON (V.toList a)
  parseJSON invalid = typeMismatch "Array" invalid

join :: String -> [String] -> String
join _ [] = ""
join comma (x:xs) = x ++ comma ++ join comma xs

sendBatchToSqs :: Stories -> IO SQS.SendMessageBatch
sendBatchToSqs (Stories ids) =
  let
    storyToEntry = (\s -> 
      let id = (T.pack . show) $ storyId s
      in SQS.sendMessageBatchRequestEntry id id)
    entries = storyToEntry <$> ids :: [SQS.SendMessageBatchRequestEntry]
  in do
    url <- getEnv "QUEUE_URL"
    traverse T.putStrLn $
      (\entry -> "sending entry: " <> T.pack (show entry)) <$> entries
    return $ L.set SQS.smbEntries entries $ SQS.sendMessageBatch (T.pack url)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (firstN, rest) = splitAt n xs 
              in [firstN] ++ chunk n rest

sendToSqs :: Story -> IO SQS.SendMessage
sendToSqs (Story id) = do
  url <- getEnv "QUEUE_URL"
  return $ SQS.sendMessage (T.pack url) (T.pack $ show id)

sendAllToSqs :: Stories -> IO [SQS.SendMessage]
sendAllToSqs ss = traverse sendToSqs $ stories ss

runRequest :: SQS.SendMessage -> IO ()
runRequest req = do
  env <- newEnv Discover
  runResourceT . runAWST env . within Ireland $ void $ send req

runRequests :: [SQS.SendMessage] -> IO ()
runRequests reqs = do
  env <- newEnv Discover
  runResourceT . runAWST env . within Ireland $ void $ traverse send reqs

sendChunkToSQS :: Stories -> IO ()
sendChunkToSQS chunk = do
  env <- newEnv Discover
  req <- sendBatchToSqs chunk
  runResourceT . runAWST env . within Ireland $ void $ send req

sendChunksToSQS :: [Stories] -> IO ()
sendChunksToSQS = void . traverse sendChunkToSQS

handler :: Value -> Context -> IO (Either String ())
handler _ context = do
  req <- parseRequest "https://hacker-news.firebaseio.com/v0/newstories.json"
  res <- httpJSON req
  let body = getResponseBody res
  let json = parseEither parseJSON body
  let chunks = fmap (fmap Stories . chunk 10 . stories) json
  traverse sendChunksToSQS chunks

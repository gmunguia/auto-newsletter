{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import Aws.Lambda
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Simple
import qualified Data.Vector as V
import qualified Data.Scientific as S

data Person = Person
  { personName :: String
  , personAge :: Int
  } deriving (Generic)
instance FromJSON Person
instance ToJSON Person

toNum :: Int -> Value
toNum n = Number $ S.scientific (toInteger n) 1

data Story = Story { id :: Int } deriving (Show)
instance ToJSON Story where
  toJSON (Story id) = toNum id

newtype Stories = Stories { stories :: [Story] } deriving (Show)
instance ToJSON Stories where
  toJSON (Stories stories) = Array $ V.fromList $ toJSON <$> stories

parseI :: Value -> Either String Int
parseI (Number v) =
  maybe (Left "Expected integer") (\i -> Right i) $
  S.toBoundedInteger v
parseI invalid = Left "Expected number"

parseA :: Value -> Either String [Int]
parseA (Array v) = (mapM parseI $ V.toList v)
parseA invalid = Left "Expected an Array"

intsToStories :: [Int] -> Stories
intsToStories ints = Stories $ Story <$> ints

handler :: Value -> Context -> IO (Either String Stories)
handler _ context =
  parseRequest "https://hacker-news.firebaseio.com/v0/newstories.json"
  >>= (\req -> (httpJSON req) )
  >>= (\res -> return $ getResponseBody res)
  >>= (\body -> return $ parseA body)
  >>= (\parsed -> return $ intsToStories <$> parsed)

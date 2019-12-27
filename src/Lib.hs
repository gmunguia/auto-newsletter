module Lib where

import Aws.Lambda
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Simple
import qualified Data.Vector as V
import qualified Data.Scientific as S

data Story = Story { id :: Int } deriving (Show)
instance ToJSON Story where
  toJSON (Story id) = Number $ S.scientific (toInteger id) 1
instance FromJSON Story where
  parseJSON (Number x) = maybe (typeMismatch "Int" $ Number x) (return . Story . Prelude.id) $ S.toBoundedInteger x
  parseJSON invalid = typeMismatch "Int" invalid

newtype Stories = Stories { stories :: [Story] } deriving (Show)
instance ToJSON Stories where
  toJSON (Stories stories) = Array $ V.fromList $ toJSON <$> stories
instance FromJSON Stories where
  parseJSON (Array a) = Stories <$> traverse parseJSON (V.toList a)
  parseJSON invalid = typeMismatch "Array" invalid

handler :: Value -> Context -> IO (Either String Stories)
handler _ context =
  parseRequest "https://hacker-news.firebaseio.com/v0/newstories.json"
  >>= httpJSON
  >>= return . getResponseBody
  >>= return . parseEither parseJSON

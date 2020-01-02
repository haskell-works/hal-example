{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Env.Options where

import Antiope.Core         hiding (LogLevel)
import Antiope.S3           (BucketName)
import Control.Applicative  ((<|>))
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Control.Monad.Logger (LogLevel (..))
import Data.Semigroup       ((<>))
import GHC.Generics         (Generic)
import System.Environment   (lookupEnv)
import Text.Read            (readEither)

import qualified Data.Text as T

data Options = Options
  { logLevel     :: LogLevel
  , region       :: Region
  , targetBucket :: BucketName
  } deriving (Show, Generic)

getOptions :: ExceptT String IO Options
getOptions = Options
  <$> (readEnv "LOG_LEVEL" <|> pure LevelInfo)
  <*> (fromTextEnv "AWS_REGION" <|> pure Oregon)
  <*> fromTextEnv "TARGET_BUCKET"

getOptionsFromEnv :: IO (Either String Options)
getOptionsFromEnv = runExceptT getOptions

readEnv :: Read a => String -> ExceptT String IO a
readEnv key = ExceptT (lookupEnvEither key) >>= (liftEither . readEither)

fromTextEnv :: FromText a => String -> ExceptT String IO a
fromTextEnv key = ExceptT (lookupEnvEither key) >>= (liftEither . fromText . T.pack)

lookupEnvEither :: String -> IO (Either String String)
lookupEnvEither key = do
  val <- lookupEnv key
  case val of
    Nothing   -> pure $ Left ("Env variable is not set: " <> key)
    Just val' -> pure $ Right val'

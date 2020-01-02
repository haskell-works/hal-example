{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Env.AppEnv where

import Antiope.Env                  (Env, Region, mkEnv)
import App.Env.Options
import Arbor.Monad.Logger           (LoggingT)
import AWS.Lambda.Context           (HasLambdaContext (..), LambdaContext (..), defConfig)
import Control.Lens                 ((^.))
import Control.Monad                (when)
import Control.Monad.Logger         (logOtherN, runStdoutLoggingT)
import Control.Monad.Trans          (lift)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Lazy         (ByteString)
import Data.Generics.Product.Typed  (typed)
import GHC.Generics                 (Generic)
import UnliftIO                     (MonadUnliftIO, withRunInIO)

import qualified Antiope.Env          as AWS
import qualified Arbor.Monad.Logger   as Log
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding   as T

data AppEnv = AppEnv
  { lambdaContext :: LambdaContext
  , options       :: Options
  , awsEnv        :: Env
  } deriving (Generic)

instance HasLambdaContext AppEnv where
  withContext ctx env = env { lambdaContext = ctx }

withAppEnv :: MonadUnliftIO m => Options -> (AppEnv -> LoggingT m ()) -> m ()
withAppEnv opts f = runStdoutLoggingT $ do
  envAws <- createAwsEnv (opts ^. typed) (opts ^. typed)
  runResourceT $ do
    appEnv <- pure $ AppEnv
      { lambdaContext = defConfig
      , options       = opts
      , awsEnv        = envAws
      }
    lift $ f appEnv

createAwsEnv :: MonadUnliftIO m => Region -> Log.LogLevel -> LoggingT m Env
createAwsEnv reg logLvl = withRunInIO $ \runInIO -> mkEnv reg (\l b -> runInIO (logAws logLvl l b))

-------------------------------------------------------------------------------
logAws :: MonadUnliftIO m => Log.LogLevel -> AWS.LogLevel -> ByteString -> LoggingT m ()
logAws lgLvl awsLvl msg = when (logLevelToAws lgLvl >= awsLvl) (logOtherN lgLvl (T.decodeUtf8 (LBS.toStrict msg)))

logLevelToAws :: Log.LogLevel -> AWS.LogLevel
logLevelToAws l = case l of
  Log.LevelError -> AWS.Error
  Log.LevelWarn  -> AWS.Error
  Log.LevelInfo  -> AWS.Error
  Log.LevelDebug -> AWS.Info
  _              -> AWS.Trace

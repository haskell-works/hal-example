{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module App.Handler where

import Antiope.Core                 (toText)
import Antiope.S3.Messages          (S3Message, messageToS3Uri)
import App.Env.AppEnv
import App.LambdaMessage
import App.Show                     (tshow)
import Arbor.Monad.Logger           (MonadLogger)
import Control.Lens                 ((^.))
import Control.Monad                (forM_)
import Control.Monad.Logger
import Control.Monad.Reader         (MonadReader, ask)
import Data.Generics.Product.Fields (field)
import Data.Text                    (Text)

handler :: (MonadReader AppEnv m, MonadLogger m)
  => LambdaMessage S3Message
  -> m Text
handler (LambdaMessage msgs) = do
  env <- ask
  logInfoN $ "Environment: "  <> tshow (env ^. field @"options")
  logInfoN $ "Context: "      <> tshow (env ^. field @"lambdaContext")
  logInfoN $ "Messages: "     <> tshow msgs
  forM_ (msgs >>= fmap messageToS3Uri) $ \uri -> do
    logInfoN $ "Processing: " <> toText uri
  pure "Done"

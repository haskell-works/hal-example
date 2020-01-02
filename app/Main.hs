{-# LANGUAGE FlexibleContexts #-}

module Main where

import App.Env.AppEnv
import App.Env.Options      (getOptionsFromEnv)
import App.Handler          (handler)
import AWS.Lambda.Runtime   (mRuntimeWithContext)
import Control.Monad.Reader
import UnliftIO.IO          (hFlush, stdout)

main :: IO ()
main = do
  mbOpts <- getOptionsFromEnv
  case mbOpts of
    Left err   -> error err
    Right opts -> withAppEnv opts $ \appEnv ->
      runReaderT (mRuntimeWithContext ((<* hFlush stdout) . handler)) appEnv

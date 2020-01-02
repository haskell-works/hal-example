{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module App.LambdaMessage where

import Antiope.Messages.Types (With (..), WithEncoded (..))
import Data.Aeson             (FromJSON)
import Data.Kind              (Type)
import GHC.Generics           (Generic)
import GHC.TypeLits

type (/) (s :: Symbol) (k :: Type) = With s k
infixr 2 /

type (//) (s :: Symbol) (k :: Type) = WithEncoded s k
infixr 2 //

newtype LambdaMessage a = LambdaMessage [[a]]
  deriving (Show, Eq, Generic)
  deriving FromJSON via ("Records" / ["body" // "Message" // "Records" / [a]])

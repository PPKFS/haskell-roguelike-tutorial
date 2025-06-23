module HsRogue.Prelude
  ( module Prelude
  , module Data.Maybe
  , module Control.Monad
  , module Control.Monad.State.Strict
  , module Rogue.Geometry.V2
  , module GHC.Generics
  , module Optics
  , Text

  , whenJust
  ) where

import Prelude

import Control.Monad
import Control.Monad.State.Strict

import Data.Maybe
import Data.Text (Text)

import GHC.Generics (Generic)

import Optics

import Rogue.Geometry.V2

whenJust :: Applicative f => Maybe a -> (a -> f b) -> f ()
whenJust mbA f = case mbA of
  Nothing -> pure ()
  Just a -> void $ f a
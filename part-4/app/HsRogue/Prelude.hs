module HsRogue.Prelude
  ( module Prelude
  , module Data.Maybe
  , module Control.Monad
  , module Control.Monad.State.Strict
  , module Rogue.Geometry.V2
  , module GHC.Generics
  , Text
  , whenJust
  ) where

import Prelude
import Data.Maybe
import Control.Monad
import Control.Monad.State.Strict
import GHC.Generics hiding (to)
import Rogue.Geometry.V2
import Data.Text (Text)

whenJust :: Applicative f => Maybe a -> (a -> f b) -> f ()
whenJust mbA f = case mbA of
  Nothing -> pure ()
  Just a -> void $ f a
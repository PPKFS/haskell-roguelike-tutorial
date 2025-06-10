module HsRogue.Prelude
  ( module Prelude
  , module Data.Maybe
  , module Control.Monad
  , module Control.Monad.State.Strict
  , module Rogue.Geometry.V2
  , module GHC.Generics
  , Text
  , whenJust
  , enumerateFromM
  , enumerateFromM_
  , showText
  ) where

import Prelude
import Data.Maybe
import Control.Monad
import Control.Monad.State.Strict
import GHC.Generics hiding (to)
import Rogue.Geometry.V2
import Data.Text (Text)
import qualified Data.Text as T

whenJust :: Applicative f => Maybe a -> (a -> f b) -> f ()
whenJust mbA f = case mbA of
  Nothing -> pure ()
  Just a -> void $ f a

enumerateFromM :: Applicative m => Int -> [a] -> (Int -> a -> m b) -> m [b]
enumerateFromM i l f = zipWithM f [i..] l

enumerateFromM_ :: Applicative m => Int -> [a] -> (Int -> a -> m b) -> m ()
enumerateFromM_ i l f = zipWithM_ f [i..] l

showText :: Show a => a -> Text
showText = T.pack . show
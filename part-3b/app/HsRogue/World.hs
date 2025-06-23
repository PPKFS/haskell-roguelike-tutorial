module HsRogue.World
  ( addActor
  , WorldState(..)
  , getPlayer
  , updateActor
  ) where

import HsRogue.Prelude

import Rogue.Monad ( MonadRogue, makeObject, MonadStore(..) )
import Rogue.Objects.Entity ( HasID(..) )
import Rogue.Objects.Object ( Object(..), ObjectKind(..) )
import Rogue.Objects.Store ( Store )
import Optics.State.Operators ( (%=), (?=) )

import HsRogue.Map hiding ( renderable )
import HsRogue.Object

import HsRogue.Renderable

data WorldState = WorldState
  { player :: ActorEntity
  , tileMap :: Tiles
  , actors :: Store Actor
  , pendingQuit :: Bool
  } deriving (Generic)

instance Monad m => MonadStore Actor (StateT WorldState m) where
  getObject e = do
    mbA <- use $ #actors % at (getID e)
    return (fromMaybe (error $ "failed to find actor with ID " <> show (getID e)) mbA)
  setObject o = #actors % at (objectId o) ?= o

addActor :: (MonadStore Actor m, MonadRogue m) => Text -> Renderable -> V2 -> m ActorEntity
addActor name r pos = do
  let objectData = ObjectData
        { position = pos
        , renderable = r
        }
  o <- makeObject (ObjectKind "actor") name objectData ()
  setObject o
  return (ActorEntity (objectId o))

getPlayer :: (MonadState WorldState m, MonadStore Actor m) => m Actor
getPlayer = do
  p <- use #player
  getObject p

updateActor :: (MonadState WorldState m, HasID a) => a -> (Actor -> Actor) -> m ()
updateActor a f = #actors % at (getID a) % _Just %= f

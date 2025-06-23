module HsRogue.World
  ( addActor
  , WorldState(..)
  , getPlayer
  , updateActor
  , getActor
  , calculateNewLocation
  ) where

import HsRogue.Prelude

import Rogue.Array2D.Boxed ((//@))
import Rogue.FieldOfView.Visibility ( emptyViewshed )
import Rogue.Monad ( MonadRogue, makeObject, MonadStore(..) )
import Rogue.Objects.Entity ( HasID(..) )
import Rogue.Objects.Object ( Object(..), ObjectKind(..) )
import Rogue.Objects.Store ( Store )
import Rogue.Tilemap (MonadTiles(..), Tilemap(..))

import Optics.State.Operators ( (%=), (?=), (.=) )

import HsRogue.Map hiding (renderable)
import HsRogue.Object
import HsRogue.Renderable

data WorldState = WorldState
  { player :: ActorEntity
  , tileMap :: Tiles
  , actors :: Store Actor
  , dirtyViewsheds :: [ActorEntity]
  , pendingQuit :: Bool
  } deriving (Eq, Ord, Generic)

instance Monad m => MonadTiles Tile (StateT WorldState m) where
  getTileM pos = use $ #tileMap % #tiles % to (`getTile` pos)
  setTileM pos t = #tileMap % #tiles % ix pos .= t
  setTilesM ts = #tileMap % #tiles %= (//@ ts)

instance Monad m => MonadStore Actor (StateT WorldState m) where
  getObject e = do
    mbA <- use $ #actors % at (getID e)
    return (fromMaybe (error $ "failed to find actor with ID " <> show (getID e)) mbA)
  setObject o = #actors % at (objectId o) ?= o

addActor :: (MonadStore Actor m, MonadRogue m) => Text -> Renderable -> V2 -> Int -> m ActorEntity
addActor name r pos viewLim = do
  let actorData = ActorData
        { objectData = ObjectData
          { position = pos
          , renderable = r
          }
        , viewshed = emptyViewshed viewLim
        }
  o <- makeObject (ObjectKind "actor") name actorData ()
  setObject o
  return (ActorEntity (objectId o))

getPlayer :: (MonadState WorldState m, MonadStore Actor m) => m Actor
getPlayer = do
  p <- use #player
  getObject p

updateActor :: (MonadState WorldState m, HasID a) => a -> (Actor -> Actor) -> m ()
updateActor a f = #actors % at (getID a) % _Just %= f

-- given a direction and a point, calculate the new point that is 1 tile in that direction.
calculateNewLocation :: Direction -> V2 -> V2
calculateNewLocation dir (V2 x y) = case dir of
  LeftDir -> V2 (x-1) y
  RightDir -> V2 (x+1) y
  UpDir -> V2 x (y-1)
  DownDir -> V2 x (y+1)
  UpRightDir -> V2 (x+1) (y-1)
  DownRightDir -> V2 (x+1) (y+1)
  UpLeftDir -> V2 (x-1) (y-1)
  DownLeftDir -> V2 (x-1) (y+1)

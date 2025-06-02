module HsRogue.World where
import HsRogue.Object
import Rogue.Objects.Store
import HsRogue.Map
import HsRogue.Prelude

data WorldState = WorldState
  { player :: ActorEntity
  , tileMap :: Tiles
  , actors :: Store Actor
  , pendingQuit :: Bool
  } deriving (Generic)
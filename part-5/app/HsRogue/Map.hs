module HsRogue.Map
  ( TileType(..)
  , Tile(..)
  , Tiles(..)
  , wallTile
  , floorTile
  , clearTile
  , placeActorOnTile
  ) where

import HsRogue.Prelude
import HsRogue.Renderable

import Rogue.Array2D.Boxed ( Array2D, (!?@) )
import Rogue.Colour ( Colour )
import Rogue.Tilemap ( TileVisibility(..), VisibilityMap(..) )
import HsRogue.Object
import qualified Data.Set as S
import Optics
import qualified Data.Map as M
import Rogue.Objects.Entity (HasID(..))

-- | We want to keep some sort of fixed set of tiles with their relevant properties.
data TileType = Floor | Wall
  deriving (Eq, Ord, Show, Generic)

-- because of immutability we don't need to worry about these being heavyweight or whatever.
data Tile = Tile
  { name :: Text
  , renderable :: Renderable
  , walkable :: Bool
  } deriving (Generic, Show)

floorTile :: Tile
floorTile = Tile "floor" floorRenderable True

wallTile :: Tile
wallTile = Tile "wall" wallRenderable False

data Tiles = Tiles
  { tiles :: Array2D Tile
  , revealedTiles :: Array2D Bool
  , tileContents :: M.Map V2 ActorEntity --(S.Set AnyEntity)
  , defaultBackgroundColour :: Colour
  } deriving (Generic, Show)

instance TileVisibility Tile where
  visibility = walkable

instance VisibilityMap Tiles where
  positionBlocksVisibility = positionBlocksVisibility . tiles

placeActorOnTile :: Actor -> Tiles -> Tiles
placeActorOnTile a = #tileContents % at (a ^. objectPosition) ?~ actorID a

clearTile :: V2 -> Tiles -> Tiles
clearTile pos = #tileContents % at pos .~ Nothing
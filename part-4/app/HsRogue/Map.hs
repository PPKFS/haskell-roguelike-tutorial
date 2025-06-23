module HsRogue.Map
  ( TileType(..)
  , Tile(..)
  , Tiles(..)
  , wallTile
  , floorTile

  ) where

import HsRogue.Prelude
import HsRogue.Renderable

import Rogue.Array2D.Boxed ( Array2D )
import Rogue.Colour ( Colour )
import Rogue.Tilemap ( TileVisibility(..), VisibilityMap(..) )

-- | We want to keep some sort of fixed set of tiles with their relevant properties.
data TileType = Floor | Wall
  deriving (Eq, Ord, Show, Generic)

-- because of immutability we don't need to worry about these being heavyweight or whatever.
data Tile = Tile
  { name :: Text
  , renderable :: Renderable
  , walkable :: Bool
  } deriving (Generic, Eq, Ord, Show)

floorTile :: Tile
floorTile = Tile "floor" floorRenderable True

wallTile :: Tile
wallTile = Tile "wall" wallRenderable False

data Tiles = Tiles
  { tiles :: Array2D Tile
  , revealedTiles :: Array2D Bool
  , defaultBackgroundColour :: Colour
  } deriving (Generic, Eq, Ord, Show)

instance TileVisibility Tile where
  visibility = walkable

instance VisibilityMap Tiles where
  positionBlocksVisibility = positionBlocksVisibility . tiles

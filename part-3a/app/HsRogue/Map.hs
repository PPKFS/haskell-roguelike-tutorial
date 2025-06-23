module HsRogue.Map
  ( TileType(..)
  , Tile(..)
  , Tiles(..)
  , wallTile
  , floorTile

  ) where

import HsRogue.Prelude
import Rogue.Array2D.Boxed ( Array2D )
import Rogue.Colour ( Colour )
import Rogue.Tilemap ( TileVisibility(..), VisibilityMap(..) )

import HsRogue.Renderable

data TileType = Floor | Wall
  deriving (Eq, Ord, Show, Generic)

data Tile = Tile
  { name :: Text
  , renderable :: Renderable
  , walkable :: Bool
  } deriving (Eq, Ord, Show, Generic)

floorTile :: Tile
floorTile = Tile "floor" floorRenderable True

wallTile :: Tile
wallTile = Tile "wall" wallRenderable False

data Tiles = Tiles
  { tiles :: Array2D Tile
  , defaultBackgroundColour :: Colour
  } deriving (Generic, Show)

instance TileVisibility Tile where
  visibility = walkable

instance VisibilityMap Tiles where
  positionBlocksVisibility = positionBlocksVisibility . tiles

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
import HsRogue.Renderable

-- | We want to keep some sort of fixed set of tiles with their relevant properties.
data TileType = Floor | Wall
  deriving (Eq, Ord, Show, Generic)

-- because of immutability we don't need to worry about these being heavyweight or whatever.
data Tile = Tile
  { tileType :: TileType
  , renderable :: Renderable
  , walkable :: Bool
  } deriving (Generic, Show)

floorTile :: Tile
floorTile = Tile Floor floorRenderable True

wallTile :: Tile
wallTile = Tile Wall wallRenderable False

data Tiles = Tiles
  { tiles :: Array2D Tile
  , defaultBackgroundColour :: Colour
  } deriving (Generic, Show)

module HsRogue.Map
  ( TileType(..)
  , Tile(..)
  , Tiles(..)
  , clearTile
  , floorTile
  , placeActorOnTile
  , wallTile
  ) where

import HsRogue.Prelude

import Rogue.Array2D.Boxed ( Array2D, (!?@) )
import Rogue.Colour ( Colour )
import Rogue.Tilemap ( TileVisibility(..), VisibilityMap(..), WalkabilityMap(..) )

import HsRogue.Object
import HsRogue.Renderable

import qualified Data.Map as M
-- | We want to keep some sort of fixed set of tiles with their relevant properties.
data TileType = Floor | Wall
  deriving (Eq, Ord, Show, Generic)

-- because of immutability we don't need to worry about these being heavyweight or whatever.
data Tile = Tile
  { tileType :: TileType
  , renderable :: Renderable
  , walkable :: Bool
  } deriving (Eq, Ord, Show, Generic)

floorTile :: Tile
floorTile = Tile Floor floorRenderable True

wallTile :: Tile
wallTile = Tile Wall wallRenderable False

data Tiles = Tiles
  { tiles :: Array2D Tile
  , revealedTiles :: Array2D Bool
  , tileContents :: M.Map V2 ActorEntity --(S.Set AnyEntity)
  , defaultBackgroundColour :: Colour
  } deriving (Eq, Ord, Show, Generic)

instance TileVisibility Tile where
  visibility = walkable

instance VisibilityMap Tiles where
  positionBlocksVisibility = positionBlocksVisibility . tiles

instance WalkabilityMap Tiles where
  positionAllowsMovement tm pos =
    let mbTileAtPosition = tiles tm !?@ pos
        tileIsWalkable = maybe False walkable mbTileAtPosition
        tileIsNotOccupied = isNothing (tileContents tm ^. at pos)
    in tileIsWalkable && tileIsNotOccupied

placeActorOnTile :: HasActorID a => V2 -> a -> Tiles -> Tiles
placeActorOnTile pos a = #tileContents % at pos ?~ actorID a

clearTile :: V2 -> Tiles -> Tiles
clearTile pos = #tileContents % at pos .~ Nothing

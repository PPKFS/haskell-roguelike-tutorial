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
import Rogue.Tilemap
import HsRogue.Object
import qualified Data.Set as S
import Optics
import qualified Data.Map as M
import Rogue.Objects.Entity (HasID(..))
import Debug.Trace (traceShow)

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
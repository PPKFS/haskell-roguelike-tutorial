module HsRogue.Map
  ( TileType(..)
  , Tile(..)
  , Tiles(..)
  , emptyRandomMap
  , testMap
  , digRooms
  , digHorizontalTunnel
  , digVerticalTunnel
  , roomsAndCorridorsMap
  ) where

import HsRogue.Prelude
import Rogue.Array2D.Boxed
import Rogue.Colour
import Rogue.Geometry.Rectangle
import System.Random.Stateful
import Data.Function
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import HsRogue.Renderable

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
  , defaultBackgroundColour :: Colour
  } deriving (Generic, Show)

emptyWallMap :: V2 -> Array2D Tile
emptyWallMap = replicateArray wallTile

emptyFloorMap :: V2 -> Array2D Tile
emptyFloorMap = replicateArray floorTile

emptyRandomMap :: Int -> V2 -> IO (Array2D Tile)
emptyRandomMap numberOfWalls size = do
  randomWalls <- mapM (const $ uniformRM (V2 0 0, size - V2 1 1) globalStdGen) [1..numberOfWalls]
  return $ emptyFloorMap size //@ map (, wallTile) (rectangleEdges (rectangleFromDimensions (V2 0 0) size) <> randomWalls)

digRoom :: Rectangle -> Array2D Tile -> Array2D Tile
digRoom rect m = m //@ map (, floorTile) (rectanglePoints Horizontal rect)

digRooms :: [Rectangle] -> Array2D Tile -> Array2D Tile
digRooms rects m = m //@ map (, floorTile) (mconcat $ map (rectanglePoints Horizontal) rects)

digHorizontalTunnel ::
  V2
  -> Int
  -> Array2D Tile
  -> Array2D Tile
digHorizontalTunnel p len x = x //@ tunnelTiles Horizontal p len

tunnelTiles ::
  ScanDirection
  -> V2
  -> Int
  -> [(V2, Tile)]
tunnelTiles dir p len = let dimAdjust = if dir == Horizontal then modifyX else modifyY in map (\i -> (dimAdjust (+ if len > 0 then i else -i) p ,floorTile)) [0..(abs len)]

digVerticalTunnel ::
  V2
  -> Int
  -> Array2D Tile
  -> Array2D Tile
digVerticalTunnel p len x = x //@ tunnelTiles Vertical p len

testMap :: V2 -> Array2D Tile
testMap =
  digHorizontalTunnel (V2 25 23) 15
  . digRoom (rectangleFromDimensions (V2 35 15) (V2 10 15))
  . digRoom (rectangleFromDimensions (V2 20 15) (V2 10 15))
  . emptyWallMap

data PlannedCorridor = HorizontalCorridor V2 Int | VerticalCorridor V2 Int

digCorridors :: [PlannedCorridor] -> Array2D Tile -> Array2D Tile
digCorridors corridors tiles = tiles //@ mconcat (map (\case
  HorizontalCorridor p len -> tunnelTiles Horizontal p len
  VerticalCorridor p len -> tunnelTiles Vertical p len) corridors)

roomsAndCorridorsMap :: Int -> Int -> Int -> V2 -> IO (Array2D Tile, NonEmpty Rectangle)
roomsAndCorridorsMap maxRooms minSize maxSize mapSize@(V2 w h) = do
  (allRooms, allCorridors) <- foldM (\acc@(existingRooms, existingCorridors) _ -> do
    dims@(V2 rW rH) <- uniformRM (V2 minSize minSize, V2 maxSize maxSize) globalStdGen
    pos <- uniformRM (V2 1 1, V2 (w - rW - 2) (h - rH - 2)) globalStdGen
    let room = rectangleFromDimensions pos dims
    let isOk = not $ any (rectanglesIntersect room) existingRooms
    case (isOk, listToMaybe existingRooms) of
      (True, Just lastRoom) -> do
        let _newCentre@(V2 nX nY) = centre room
            oldCentre@(V2 oX oY) = centre lastRoom
        digHorizontal <- uniformM @Bool globalStdGen
        let twoCorridors = if digHorizontal
              then [HorizontalCorridor oldCentre (nX - oX), VerticalCorridor (V2 nX oY) (nY - oY)]
              else [VerticalCorridor oldCentre (nY - oY), HorizontalCorridor (V2 oX nY) (nX - oX)]
        pure (room:existingRooms, twoCorridors <> existingCorridors)
      (True, Nothing) -> return (room:existingRooms, existingCorridors)
      _ -> return acc

    ) ([], []) [1..maxRooms]
  return (digRooms allRooms (emptyWallMap mapSize) & digCorridors allCorridors, fromMaybe (error "room generator made 0 rooms") $ NE.nonEmpty allRooms)

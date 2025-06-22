---
author: ["Avery"]
title: "Haskell Roguelike Tutorial Part 2 - Making a Map"
date: "2025-04-18"
modified: "2025-06-19"
description: "This is the first 'proper' tutorial post; there's a walkthrough of the window opening example before jumping in with moving an @ around the screen."
summary: "Walking through the window opening code and drawing and moving a character."
tags: ["roguelike", "tutorial", "projects", "haskell"]
categories: ["haskell"]
series: ["roguelike-tutorial"]
ShowToc: true
TocOpen: true
draft: false
weight: 1
social:
  bluesky: "ppkfs@bsky.social"
---

```haskell
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
```

```haskell
module HsRogue.MapGen
  ( emptyRandomMap
  , testMap
  , roomsAndCorridorsMap
  , digRooms
  , digRoom
  , digCorridors
  , PlannedCorridor(..)
  , digHorizontalTunnel
  , digVerticalTunnel

  ) where

import HsRogue.Prelude

import Data.List.NonEmpty (NonEmpty)
import HsRogue.Map

import Rogue.Array2D.Boxed ( (//@), replicateArray, Array2D )
import Rogue.Geometry.Rectangle
    ( centre
    , rectangleEdges
    , rectangleFromDimensions
    , rectanglesIntersect
    , Orientation(..)
    , Rectangle
    , rectanglePoints'
    )
import Rogue.Monad ( MonadRogue )
import Rogue.Random ( coinFlip, randomPoint, randomV2 )
import qualified Data.List.NonEmpty as NE

emptyWallMap :: V2 -> Array2D Tile
emptyWallMap = replicateArray wallTile

emptyFloorMap :: V2 -> Array2D Tile
emptyFloorMap = replicateArray floorTile

emptyRandomMap :: MonadRogue m => Int -> V2 -> m (Array2D Tile)
emptyRandomMap numberOfWalls size = do
  randomWalls <- mapM (const $ randomPoint (V2 0 0) size) [1..numberOfWalls]
  let outsideBorders = rectangleEdges (rectangleFromDimensions (V2 0 0) size)
      allWalls = outsideBorders <> randomWalls
      asWallTiles = map (, wallTile) allWalls
  return $ emptyFloorMap size //@ asWallTiles

-- | Given an orientation, a start, and a length - make a line of floor tiles
tunnelTiles ::
  Orientation
  -> V2
  -> Int
  -> [(V2, Tile)]
tunnelTiles dir p len =
  let dimAdjust = if dir == Horizontal then modifyX else modifyY
      extendingBy index a = if len > 0 then a + index else a + (-index)
  in map (\i -> (dimAdjust (extendingBy i) p ,floorTile)) [0..(abs len)]

digRoom :: Rectangle -> Array2D Tile -> Array2D Tile
digRoom rect m = m //@ map (, floorTile) (rectanglePoints' rect)

digRooms :: [Rectangle] -> Array2D Tile -> Array2D Tile
digRooms rects m = m //@ map (, floorTile) (mconcat $ map rectanglePoints' rects)

digHorizontalTunnel ::
  V2
  -> Int
  -> Array2D Tile
  -> Array2D Tile
digHorizontalTunnel p len x = x //@ tunnelTiles Horizontal p len

digVerticalTunnel ::
  V2
  -> Int
  -> Array2D Tile
  -> Array2D Tile
digVerticalTunnel p len x = x //@ tunnelTiles Vertical p len

data PlannedCorridor = HorizontalCorridor V2 Int | VerticalCorridor V2 Int

digCorridors :: [PlannedCorridor] -> Array2D Tile -> Array2D Tile
digCorridors corridors tiles =
  let corridorTiles = \case
        HorizontalCorridor p len -> tunnelTiles Horizontal p len
        VerticalCorridor p len -> tunnelTiles Vertical p len
  in tiles //@ mconcat (map corridorTiles corridors)

roomsAndCorridorsMap :: MonadRogue m => Int -> Int -> Int -> V2 -> m (Array2D Tile, NonEmpty Rectangle)
roomsAndCorridorsMap maxRooms minSize maxSize mapSize@(V2 w h) = do

  (allRooms, allCorridors) <- foldM (\acc@(existingRooms, existingCorridors) _ -> do
    room <- makeOneRandomRoom
    let isOk = not $ any (rectanglesIntersect room) existingRooms

    case (isOk, existingRooms) of
      (True, lastRoom:_) -> do
        let V2 nX nY = centre room
            oldCentre@(V2 oX oY) = centre lastRoom
        digHorizontal <- coinFlip

        let twoCorridors = if digHorizontal
              then [HorizontalCorridor oldCentre (nX - oX), VerticalCorridor (V2 nX oY) (nY - oY)]
              else [VerticalCorridor oldCentre (nY - oY), HorizontalCorridor (V2 oX nY) (nX - oX)]

        return (room:existingRooms, twoCorridors <> existingCorridors)
      (True, []) -> return (room:existingRooms, existingCorridors)

      _ -> return acc
    ) ([], []) [1..maxRooms]
  let mapWithDugRooms = digCorridors allCorridors . digRooms allRooms $ emptyWallMap mapSize
  return (mapWithDugRooms, fromMaybe (error "room generator made 0 rooms") $ NE.nonEmpty allRooms)

  where
    makeOneRandomRoom = do
      dims@(V2 rW rH) <- randomV2 (V2 minSize minSize) (V2 maxSize maxSize)
      pos <- randomV2 (V2 1 1) (V2 (w - rW - 2) (h - rH - 2))
      return $ rectangleFromDimensions pos dims

testMap :: V2 -> Array2D Tile
testMap =
  digHorizontalTunnel (V2 25 23) 15
  . digRoom (rectangleFromDimensions (V2 35 15) (V2 10 15))
  . digRoom (rectangleFromDimensions (V2 20 15) (V2 10 15))
  . emptyWallMap
```

```haskell
module HsRogue.Renderable
  ( Renderable(..)
  , playerRenderable
  , floorRenderable
  , wallRenderable
  ) where

import HsRogue.Prelude
import Rogue.Colour ( ivory, lightSlateGray, mediumSeaGreen, Colour )

-- Somethign that represents a renderable character. Just one.
data Renderable = Renderable
  { glyph :: Char
  , foreground :: Colour
  } deriving (Show, Read, Generic)

playerRenderable :: Renderable
playerRenderable = Renderable '@' ivory

floorRenderable :: Renderable
floorRenderable = Renderable '.' lightSlateGray

wallRenderable :: Renderable
wallRenderable = Renderable '#' mediumSeaGreen
```

```haskell
type GameMonad m = (MonadRogue m, MonadIO m, MonadState WorldState m)

data WorldState = WorldState
  { playerPosition :: V2
  , tileMap :: Tiles
  , pendingQuit :: Bool
  }
```

```haskell
main :: IO ()
main = do
  withWindow
    defaultWindowOptions { size = Just screenSize }
    initGame
    (evalStateT runLoop)
    (return ())

initGame :: MonadRogue m => m WorldState
initGame = do
  (madeMap, firstRoom:|_) <- roomsAndCorridorsMap 30 4 12 screenSize
  return $
    WorldState
      { playerPosition = centre firstRoom
      , tileMap = Tiles madeMap black
      , pendingQuit = False
      }
```
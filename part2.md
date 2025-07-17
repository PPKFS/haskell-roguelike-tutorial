---
author: ["Avery"]
title: "Haskell Roguelike Tutorial Part 2 - Making a Map"
date: "2025-07-09"
modified: "2025-07-09"
description: "Back for Part 2, this part introduces a proper map structure that we can move around in. Then we make a couple of very simple maps; one with random walls and one with rooms and corridors."
summary: "Part 2: Making some maps with rooms and corridors."
tags: ["roguelike", "tutorial", "projects", "haskell"]
categories: ["haskell"]
series: ["roguelike-tutorial"]
ShowToc: true
TocOpen: true
draft: true
weight: 1
social:
  bluesky: "ppkfs@bsky.social"
---

Welcome back to part 2 of the Haskell roguelike tutorial! In the [previous part](https://ppkfs.github.io/posts/roguelike-tutorial/part1/) we added a player that we could move around with the arrow keys and WASD.
This isn't particularly fun or exciting, so in this part we're going to add a basic map to move around in. This also marks the start of 3 parts (well, 2 parts - but one is split in two) of designing our game's architecture. In this part we're going to add a tile map, and in the next part we're going to add objects (rather than treating the player as just a graphic with a position) and also introduce `optics` to finally deal with the headache of nested record updates.

As we're going to be introducing a bunch more modules and importing a bunch from `roguefunctor`, I will be condensing some of the code snippets compared to the [example repository](https://github.com/ppkfs/haskell-roguelike-tutorial) - I'll cut out explicit import and export lists. It's good practice to make these explicit - and the full code provided will have them to make it clear what is imported from where - but it gets a bit messy to keep updating the imports in each snippet here.

# Optional: Upgrading our font

`BearLibTerminal` ships with a default font built-in (literally; it's hard-coded in as a raw string of bytes) which is *fine* but it's a little ugly. It also has the minor annoyance that, like most fonts, it's not square. This is fine for writing, but less good for a tile-based game. It's very noticeable if you make diagonal lines, where you seem to be going vertically twice as far as horizontally. One font I particularly like is [**Kreative Square**](https://www.kreativekorp.com/software/fonts/ksquare/). It's a monospace square font that looks pretty good, has a bunch of special characters that are handy for roguelikes (box drawing, shapes, and the like) and -- most importantly for this tutorial -- has a very permissive license. The font is completely free to download [here](https://github.com/kreativekorp/open-relay). The libraries support any bitmap (.bmp) or TrueType (.ttf) font. For full details of configuration options, check the [BearLibTerminal configuration documentation](http://foo.wyrd.name/en:bearlibterminal:reference:configuration).

All we need to do to use this new font is to place the `KreativeSquare.ttf` file in the root folder of our project and then tell the library the font we wish to use as the default:


```haskell
main :: IO ()
main = do
  withWindow
    defaultWindowOptions { size = Just screenSize }
    initGame
    (const $ evalStateT runLoop (WorldState initialPlayerPosition False))
    (return ())

initGame :: MonadRogue m => m ()
initGame = do
  terminalSet_ "font: KreativeSquare.ttf, size=16x16"
```

The only required parameter for `.ttf` fonts is the size to be rendered at (in pixels). As we don't need to set any alignment -- it will all be calculated automatically -- it's just plug and play. If you're not a fan of how stringly-configured `BearLibTerminal` is, there exist nice typed versions too:

```haskell
initGame :: MonadRogue m => m ()
initGame = do
  terminalSetOptions_ $ defaultTrueTypeFontOptions { font = "KreativeSquare.ttf", size = V2 16 16 }
```

which will function identically. If you run it now, you should immediately see the window has grown somewhat (because we're now using a bigger font) and also it looks slightly less dated. If you're wondering about having graphical tiles, worry not! They are, actually, very easy to do without having to remember whether "g" maps to a corner wall or an edge wall. Just for a later part. :)

# Making an (empty) map type

## The Renderable type

So let's think about what we need from our map type. A map should be some sort of 2D array of Tiles, where each Tile has some properties -- for example, the colour and character to use to render it, and what type of tile it is (right now, that'll be a floor or a wall). Ah, but we also need at least some of this for rendering other things -- such as the player. Let's make a type to represent anything renderable; nothing complicated, just a character and a colour. In `HsRogue.Renderable`:

```haskell
module HsRogue.Renderable where

import HsRogue.Prelude
import Rogue.Colour ( ivory, lightSlateGray, mediumSeaGreen, Colour )

data Renderable = Renderable
  { glyph :: Char
  , foreground :: Colour
  } deriving (Eq, Ord, Show, Generic)
```

Fairly straightforward. We can also now define the three renderables we want to use currently (the player, a wall, and the floor). The colours I've picked are purely arbitrary - in `Rogue.Colour` you'll find functions for making and modifying colours based on RGB values as well as the 140 named colours from the HTML specification to save you writing in hexcodes - for an overview, you can see them all here - https://www.w3schools.com/tags/ref_colornames.asp. Colours are simply represented as 32-bit integers under the hood, so you can define your own colours with hex notation directly -- such as `Colour 0xFFFF69B4` for hot pink.


```haskell

playerRenderable :: Renderable
playerRenderable = Renderable '@' ivory

floorRenderable :: Renderable
floorRenderable = Renderable '.' lightSlateGray

wallRenderable :: Renderable
wallRenderable = Renderable '#' mediumSeaGreen
```

I've gone for a simple off-white for the player, a light grey for the floor, and a green for the walls.

## The Tile type

Let's make another new module and call it `HsRogue.Map`. We'll start with a couple of types for each individual tile. We don't actually *need* the `TileType`, because we store the properties of the tile independently. However, it does make things easier to debug when you need to `print` a tile to see why something isn't working! The alternative - where a tile is actually just a `TileType` and `walkable` and `renderable` become functions that look like `TileType -> Bool` and `TileType -> Renderable` respectively - would work too, but it would mean we cannot ever have a 'different' floor tile. The intent here is to have `TileType` be a generic classification into floors or walls or pits or grass or whatever, but to determine if something is walkable is done per-tile.


```haskell
module HsRogue.Map where

import HsRogue.Prelude
import HsRogue.Renderable

data TileType = Floor | Wall
  deriving (Eq, Ord, Show, Generic)

data Tile = Tile
  { tileType :: TileType
  , renderable :: Renderable
  , walkable :: Bool
  } deriving (Generic, Show)

floorTile :: Tile
floorTile = Tile Floor floorRenderable True

wallTile :: Tile
wallTile = Tile Wall wallRenderable False
```

We can do a similar thing to `Renderable` here, where we can define the single copy of a generic wall tile and a floor tile. Thanks, immutability!

## At last, a tilemap

Then for making the actual map, we can use the `Rogue.Array2D.Boxed` type. It's a convenient wrapper around `vector` (which are contiguous-in-memory arrays with O(1) lookup time) that allows us to pretend it's a 2D array that we can index with a 2D coordinate (`V2`). There are a few different 2D array implementations in the library - `Boxed` is the one you'll probably want, as it allows for making arrays of any sort of Haskell type. There's also `Array2D.Unboxed`, roughly for types that are 'primitive' (such as integers) which has better performance, and a version that uses *mutable* vectors.

A warning: the one downside of `vector` in Haskell is that making a change means the **entire vector has to be copied**. That makes updating a vector (or an `Array2D`) potentially quite slow. For the most part as long as you're not doing it hundreds of times a second, you should have no problem! One way we will try to avoid this, especially for building the map, is to combine all of our update operations into one. There's even an operator - `@// :: Array2D a -> [(V2, a)] -> Array2D a` - for doing bulk updates.

```haskell
import Rogue.Array2D.Boxed ( Array2D )
import Rogue.Colour ( Colour )

data Tiles = Tiles
  { tiles :: Array2D Tile
  , defaultBackgroundColour :: Colour
  } deriving (Generic, Show)
```

We also add a field for the background colour just to avoid any rendering bugs further down the line. We could hardcode a `terminalBkColour black` but this avoids us forgetting. Let's add an empty map to our `WorldState` and render it. In `Main.hs`:

```haskell
data WorldState = WorldState
  { playerPosition :: V2
  , tileMap :: Tiles -- we added this
  , pendingQuit :: Bool
  }

main :: IO ()
main = do
  withWindow
    defaultWindowOptions { size = Just screenSize }
    initGame
    (evalStateT runLoop)
    (return ())

initGame :: m WorldState
initGame = do
  return $
    WorldState
      { playerPosition = initialPlayerPosition
      , tileMap = replicateArray floorTile screenSize
      , pendingQuit = False
      }
```

`replicateArray` makes a new array filled with copies of the given element (in this case, floor tiles) with a given size.

If you updated your font, the `initGame` function won't be new. If you didn't, then we're simply moving the world creation logic into its own function. It may seem redundant that it is a *monadic* function when we just return pure data - and that's correct, but very shortly (when we want to use some randomness to make our map) we'll need to be in `MonadIO` anyway. The last part is to render the tile map. We need to iterate over every tile in the array (along with its coordinates), set the currently active foreground and background colours, before drawing the relevant tile at its coordinates. As we only have one background colour for the entire map (for now), we can set this just once at the start of the rendering call. We can use `traverseArrayWithCoord` to iterate an array and perform some operation:

```haskell

traverseArrayWithCoord ::
  Monad m -- we're wanting to map over the array with some sort of side effects, so it is collected in a monad context
  => Array2D a  -- given an array of `a`s..
  -> (V2 -> a -> m b) -- and a function that produces a `b` (with some monadic effects, potentially)
  -> m (V.Vector b) -- sequence all those effects and return a new vector with the new elements.
```

If you don't need to calculate the returned vector (e.g. you just want to do some effect like printing, meaning your function looks like `V2 -> a -> m ()` and you'd be producing a `V.Vector ()`) you can ignore it with `traverseArrayWithCoord_` like we do here. Similarly, if you don't need the coordinate of the element you can use `traverseArray/traverseArray_`.

```haskell
runLoop :: GameMonad m => m ()
runLoop = do
  terminalClear
  renderMap
  ...

renderMap :: GameMonad m => m ()
renderMap = do
  w <- get
  terminalBkColour (defaultBackgroundColour (tileMap w))
  traverseArrayWithCoord_ (tiles (tileMap w)) $ \p Tile{..} -> do
    terminalColour (foreground renderable)
    printChar p (glyph renderable)
```

Here I'm using `RecordWildCards` to shortcut manually expanding the `Tile` constructor. You can either add it to your `default-extensions` in your `*.cabal` file, or add `{-# LANGUAGE RecordWildCards #-}` at the top of the module. We set the active background colour once, and then for each individual tile we set the active foreground colour before printing it at its coordinate. We then add the rendering to the main loop.

If you compile and run it now, you'll notice the entire world is now floor and you can walk around on it! However, it's quite boring and you can still proceed to walk off the side of the screen. Let's make it more exciting.

# Making an interesting map

We'll make a new module for our various map generators. In a much later part, we'll want to revisit these to make them truly modular and combinable. We'll put all of our map generator utilities in `HsRogue.MapGen`.

```haskell

module HsRogue.MapGen where

import HsRogue.Prelude

import HsRogue.Map

import Rogue.Array2D.Boxed ( replicateArray, Array2D )


emptyWallMap :: V2 -> Array2D Tile
emptyWallMap = replicateArray wallTile

emptyFloorMap :: V2 -> Array2D Tile
emptyFloorMap = replicateArray floorTile
```

Let's start with the basics: we'll have both sorts of empty (or filled, I suppose) map. We already saw a use for the empty *floor* map, and we'll use it shortly for placing random walls - we'll start with all floor and then place walls. The empty *wall* map is handy for doing the opposite - starting with all the walls and digging out some rooms and corridors.


```haskell
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
```

## Map 1: Random walls

Our first goal will be to write a function that takes in some dimensions and a number of walls and places random wall tiles. We'll also add a border around the entire map. Emphasis on the word 'random'. If you're not aware of how randomness works in programming, in short:
-  You provide some starting value (the seed). Giving the same seed will give the same sequence of random numbers every time. Sometimes you'll want to specify this, but normally it's generated off the current time.
- This seed value goes into a complicated calculation that generates a state (a big number) for the generator. This is often called the 'generator state'.
- Every time you want to generate a random number, the generator state is plugged into that complicated calculation. This spits out a random number and a new value for the generator state. Then, you update the generator state to this new value.

It's **not** actually random: however to the human eye, the numbers seem to be completely random.

There's usually two options for doing (pseudo-)randomness in Haskell:

- You can manually keep track of your generator state `g`. You then use **pure** functions that look like `random :: g -> (a, g)` - given a generator state, produce a newly updated generator state and a random `a`.
- You can use a global variable that keeps track of the generator state for you. As this global variable lives in the `IO` monad, all your functions for doing randomness are **in the IO monad** - they look like `randomM :: IO a`.

`Roguefunctor` does a bit of both. There *is* a global-ish random number generator, but instead of being in the `IO` monad it is stored in the library in the **RogueT** monad transformer. Remember back to `withWindow` where we said that the "main loop" parameter was not `IO` but instead something else with `IO` in it? This is why - we keep a separate state for things like the RNG state. The `RogueT` transformer implements the `MonadRogue` typeclass,


## Map 2: I am a dwarf and I'm digging some rooms and corridors

## Map 3: An actual...dungeon?

# Wrapping up

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


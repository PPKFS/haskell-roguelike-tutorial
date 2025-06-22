---
author: ["Avery"]
title: "Haskell Roguelike Tutorial Part 3a - Adding Objects"
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
instance TileVisibility Tile where
  visibility = walkable

instance VisibilityMap Tiles where
  positionBlocksVisibility = positionBlocksVisibility . tiles
```

```haskell
module HsRogue.Object
  ( objectRenderable
  , objectPosition
  , moveObject
  , Actor
  , ActorEntity(..)
  , ObjectData(..)
  ) where

import HsRogue.Prelude
import HsRogue.Renderable
import Rogue.Objects.Object as RF ( Object(..) )
import Rogue.Objects.Entity ( Entity(..) )

data ObjectData = ObjectData
  { position :: V2
  , renderable :: Renderable
  }

type Actor = RF.Object ObjectData ()

newtype ActorEntity = ActorEntity { unActor :: Entity }
  deriving (Eq, Ord, Show, Enum)

objectRenderable :: RF.Object ObjectData a -> Renderable
objectRenderable = renderable . objectData

objectPosition :: RF.Object ObjectData a -> V2
objectPosition = position . objectData

moveObject :: V2 -> RF.Object ObjectData a -> RF.Object ObjectData a
moveObject pos o = o { objectData = (objectData o) {position = pos } }
```

```haskell
module HsRogue.World
  ( addActor
  , WorldState(..)
  , getPlayer
  , updateActor
  ) where

import HsRogue.Prelude

import Data.Coerce (coerce)
import HsRogue.Map hiding (renderable)
import HsRogue.Object

import HsRogue.Renderable
import Rogue.Monad ( MonadRogue, makeObject )
import Rogue.Objects.Entity ( Entity(..), HasID(..) )
import Rogue.Objects.Object ( Object(..), ObjectKind(..) )
import Rogue.Objects.Store ( Store, unsafeLookup, update, insert )

data WorldState = WorldState
  { player :: ActorEntity
  , tileMap :: Tiles
  , actors :: Store Actor
  , pendingQuit :: Bool
  } deriving (Generic)

addActor :: (MonadState WorldState m, MonadRogue m) => Text -> Renderable -> V2 -> m ActorEntity
addActor name r pos = do
  let objectData = ObjectData
        { position = pos
        , renderable = r
        }
  o <- makeObject (ObjectKind "actor") name objectData ()
  acStore <- gets actors
  let newStore = insert (objectId o) o acStore
  modify (\w -> w { actors = newStore })
  return (ActorEntity (objectId o))

getPlayer :: MonadState WorldState m => m Actor
getPlayer = do
  w <- get
  return $ unsafeLookup (coerce $ player w) (actors w)

updateActor :: (MonadState WorldState m, HasID a) => a -> (Actor -> Actor) -> m ()
updateActor a f = modify (\w -> w { actors = update (coerce $ getID a) f (actors w) })
```
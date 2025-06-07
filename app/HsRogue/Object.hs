module HsRogue.Object
  ( objectRenderable
  , objectPosition
  , moveObject
  , Actor
  , ActorEntity(..)
  , ObjectData(..)

  ) where

import HsRogue.Prelude
import HsRogue.Renderable (Renderable)
import Rogue.Objects.Object as RF
import Rogue.Objects.Entity

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
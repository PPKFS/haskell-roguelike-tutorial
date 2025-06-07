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
import Rogue.Objects.Entity ( Entity(..), HasID(..) )

import Optics

data ObjectData = ObjectData
  { position :: V2
  , renderable :: Renderable
  } deriving (Show, Eq, Ord, Generic)

type Actor = RF.Object ObjectData ()

newtype ActorEntity = ActorEntity { unActor :: Entity }
  deriving (Eq, Ord, Show, Enum)

instance HasID ActorEntity where
  getID = unActor

objectRenderable :: Lens' (RF.Object ObjectData a) Renderable
objectRenderable = #objectData % #renderable

objectPosition :: Lens' (RF.Object ObjectData a) V2
objectPosition = #objectData % #position

moveObject :: V2 -> RF.Object ObjectData a -> RF.Object ObjectData a
moveObject pos = objectPosition .~ pos

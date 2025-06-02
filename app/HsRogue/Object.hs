module HsRogue.Object where

import HsRogue.Prelude
import HsRogue.Renderable (Renderable)
import qualified Rogue.Objects.Object as RF
import Rogue.Objects.Entity
import Rogue.Objects.Tag

data ObjectData = ObjectData
  { position :: V2
  , renderable :: Renderable
  }

type Actor = RF.Object ObjectData ()

newtype ActorEntity = ActorEntity { unActor :: Entity }
  deriving (Eq, Ord, Show, Enum)

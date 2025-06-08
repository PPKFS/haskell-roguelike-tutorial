module HsRogue.Object
  ( objectRenderable
  , objectPosition
  , moveObject
  , HasActorID(..)
  , getActor
  , Actor
  , ActorData(..)
  , ActorEntity(..)
  , ObjectData(..)
  , HasObjectData(..)
  , Direction(..)
  , playerKind
  , actorKind
  , monsterKind
  ) where

import HsRogue.Prelude
import HsRogue.Renderable
import Rogue.Objects.Object as RF ( Object(..), ObjectKind (..) )
import Rogue.Objects.Entity ( Entity(..), HasID(..) )

import Optics
import Rogue.FieldOfView.Visibility (Viewshed)
import Rogue.Monad (MonadStore, getObject)

data ActorData = ActorData
  { objectData :: ObjectData
  , viewshed :: Viewshed
  } deriving (Show, Eq, Ord, Generic)

data ObjectData = ObjectData
  { position :: V2
  , renderable :: Renderable
  } deriving (Show, Eq, Ord, Generic)

class HasObjectData o where
  objectDataL :: Lens' o ObjectData

class HasActorID o where
  actorID :: o -> ActorEntity

type Actor = RF.Object ActorData ()

instance HasActorID Actor where
  actorID = ActorEntity . getID

instance HasActorID ActorEntity where
  actorID = id

instance HasObjectData (RF.Object ObjectData a) where
  objectDataL = #objectData

instance HasObjectData Actor where
  objectDataL = #objectData % #objectData

newtype ActorEntity = ActorEntity { unActor :: Entity }
  deriving (Eq, Ord, Show, Enum)

instance HasID ActorEntity where
  getID = unActor

data Direction = LeftDir | RightDir | UpDir | DownDir | UpRightDir | DownRightDir | UpLeftDir | DownLeftDir
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

objectRenderable :: HasObjectData o => Lens' o Renderable
objectRenderable = objectDataL % #renderable

objectPosition :: HasObjectData o => Lens' o V2
objectPosition = objectDataL % #position

moveObject :: HasObjectData o => V2 -> o -> o
moveObject pos = objectPosition .~ pos

getActor :: MonadStore Actor m => ActorEntity -> m Actor
getActor = getObject

actorKind :: ObjectKind
actorKind = ObjectKind "actor"

monsterKind :: ObjectKind
monsterKind = ObjectKind "monster"

playerKind :: ObjectKind
playerKind = ObjectKind "player"
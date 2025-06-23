module HsRogue.Object
  ( Actor
  , ActorData(..)
  , ActorEntity(..)
  , Direction(..)
  , HasActorID(..)
  , HasObjectData(..)
  , ObjectData(..)
  , getActor
  , moveObject
  , objectPosition
  , objectRenderable
  ) where

import HsRogue.Prelude
import HsRogue.Renderable
import Rogue.Objects.Object as RF ( Object(..) )
import Rogue.Objects.Entity ( Entity(..), HasID(..) )

import Rogue.FieldOfView.Visibility (Viewshed)
import Rogue.Monad (MonadStore, getObject)

data ActorData = ActorData
  { objectData :: ObjectData
  , viewshed :: Viewshed
  } deriving (Eq, Ord, Show, Generic)

data ObjectData = ObjectData
  { position :: V2
  , renderable :: Renderable
  } deriving (Eq, Ord, Show, Generic)

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
  deriving  (Eq, Ord, Show, Generic, Enum)

instance HasID ActorEntity where
  getID = unActor

data Direction = LeftDir | RightDir | UpDir | DownDir | UpRightDir | DownRightDir | UpLeftDir | DownLeftDir
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

objectRenderable :: HasObjectData o => Lens' o Renderable
objectRenderable = objectDataL % #renderable

objectPosition :: HasObjectData o => Lens' o V2
objectPosition = objectDataL % #position

moveObject :: HasObjectData o => V2 -> o -> o
moveObject pos = objectPosition .~ pos

getActor :: MonadStore Actor m => ActorEntity -> m Actor
getActor = getObject

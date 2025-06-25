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
  , MonsterSpecifics(..)
  , MonsterBehaviour(..)
  , ActorSpecifics(..)
  , Direction(..)
  , playerKind
  , actorKind
  , monsterKind
  ) where

import HsRogue.Prelude

import Rogue.Objects.Object as RF ( Object(..), ObjectKind (..) )
import Rogue.Objects.Entity ( Entity(..), HasID(..) )

import Rogue.FieldOfView.Visibility (Viewshed)
import Rogue.Monad (MonadStore, getObject)

import HsRogue.Renderable

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

type Actor = RF.Object ActorData ActorSpecifics

data ActorSpecifics =
  PlayerSpecifics ()
  | MonsterS MonsterSpecifics
  deriving (Eq, Ord, Show, Generic)

data MonsterSpecifics = MonsterSpecifics
  { insult :: Text
  , behaviour :: MonsterBehaviour
  , seenPlayer :: Bool
  } deriving (Eq, Ord, Show, Generic)

data MonsterBehaviour = AttackPlayer | FleeFromPlayer
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

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

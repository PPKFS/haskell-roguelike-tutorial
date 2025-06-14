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

  , CombatStats(..)
  , playerCombatStats
  , goblinCombatStats
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
  , combat :: Combat
  , viewshed :: Viewshed
  } deriving (Show, Eq, Ord, Generic)

data CombatStats = CombatStats
  { maxHp :: Int
  , currentHp :: Int
  , attack :: Int
  , defense :: Int
  } deriving (Show, Eq, Ord, Generic)

data ObjectData = ObjectData
  { position :: V2
  , renderable :: Renderable
  } deriving (Show, Eq, Ord, Generic)

class HasObjectData o where
  objectDataL :: Lens' o ObjectData

class HasActorID o where
  actorID :: o -> ActorEntity

type Actor = RF.Object ActorData ActorSpecifics

data ActorSpecifics =
  PlayerSpecifics ()
  | MonsterS MonsterSpecifics
  deriving (Show, Eq, Ord, Generic)

data MonsterSpecifics = MonsterSpecifics
  { insult :: Text
  , seenPlayer :: Bool
  } deriving (Show, Eq, Ord, Generic)

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

goblinCombatStats :: CombatStats
goblinCombatStats =
  CombatStats
    { maxHp = 10
    , currentHp = 10
    , attack = 3
    , defense = 1
    }

playerCombatStats :: CombatStats
playerCombatStats =
  CombatStats
    { maxHp = 30
    , currentHp = 30
    , attack = 5
    , defense = 2
    }
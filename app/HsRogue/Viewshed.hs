module HsRogue.Viewshed
  ( getVisibleTiles
  , makeAllViewshedsDirty
  , moveActorInDirection
  , moveActor
  , updateViewsheds
  ) where

import HsRogue.Prelude

import Optics.State.Operators ( (%=), (.=) )
import Rogue.Array2D.Boxed ((//@))
import Rogue.FieldOfView.Raycasting ( calculateFov )
import Rogue.FieldOfView.Visibility ( Viewshed(..) )
import Rogue.Monad ( modifyObject, traverseObjects_, MonadStore )

import HsRogue.Map
import HsRogue.Object
import HsRogue.World

import qualified Data.Set as S

makeAllViewshedsDirty :: (MonadStore Actor m, MonadState WorldState m) => m ()
makeAllViewshedsDirty = traverseObjects_ (use #actors) $ \actor -> makeViewshedDirty (actorID actor) >> return Nothing

makeViewshedDirty :: MonadState WorldState m => ActorEntity -> m ()
makeViewshedDirty v = #dirtyViewsheds %= (v:)

updateViewsheds :: (MonadStore Actor m, MonadState WorldState m) => m ()
updateViewsheds = do
  vs <- use #dirtyViewsheds
  #dirtyViewsheds .= []
  mapM_ updateViewshed vs

updateViewshed :: (MonadStore Actor m, MonadState WorldState m) => ActorEntity -> m ()
updateViewshed e = do
  o <- getActor e
  p <- use #player
  tm <- use #tileMap
  let viewshed = o ^. #objectData % #viewshed
  let fov = calculateFov tm (o ^. objectPosition) (range viewshed)
  updateActor o (#objectData % #viewshed % #visibleTiles .~ fov)
  when (p == e) $ do
    let newlyRevealedTiles = map (,True) (S.toList fov)
    #tileMap % #revealedTiles %= (//@ newlyRevealedTiles)

getVisibleTiles :: (MonadStore Actor m, MonadState WorldState m) => m (S.Set V2)
getVisibleTiles = do
  pl <- getPlayer
  return (pl ^. #objectData % #viewshed % #visibleTiles)

moveActor :: (MonadStore Actor m, MonadState WorldState m, HasActorID a) => a -> V2 -> m ()
moveActor e newPos = do
  o <- getActor (actorID e)
  modifyObject o (objectPosition .~ newPos)
  #tileMap %= clearTile (o ^. objectPosition)
  #tileMap %= placeActorOnTile newPos o
  makeViewshedDirty (actorID e)

moveActorInDirection :: (MonadStore Actor m, MonadState WorldState m, HasActorID a) => a -> Direction -> m ()
moveActorInDirection e dir = do
  o <- getActor (actorID e)
  let newPos = calculateNewLocation dir $ o ^. objectPosition
  moveActor o newPos

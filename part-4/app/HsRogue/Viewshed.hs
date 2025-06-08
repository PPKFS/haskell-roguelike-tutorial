module HsRogue.Viewshed where

import HsRogue.Prelude
import HsRogue.World
import HsRogue.Object
import Rogue.FieldOfView.Visibility
import Rogue.FieldOfView.Raycasting
import qualified Data.Set as S
import Rogue.Array2D.Boxed ((//@))
import Optics.State.Operators
import Rogue.Monad
import Optics
import HsRogue.Map

makeAllViewshedsDirty :: (MonadStore Actor m, MonadState WorldState m) => m ()
makeAllViewshedsDirty = traverseObjects_ (use #actors) $ \actor -> makeViewshedDirty (actorID actor) >> return Nothing

makeViewshedDirty :: MonadState WorldState m
  => ActorEntity
  -> m ()
makeViewshedDirty v = #dirtyViewsheds %= (v:)

updateViewsheds :: MonadIO m
  => (MonadStore Actor m, MonadState WorldState m)
  => m ()
updateViewsheds = do
  vs <- use #dirtyViewsheds
  #dirtyViewsheds .= []
  forM_ vs updateViewshed

updateViewshed ::
  MonadState WorldState m
  => MonadStore Actor m
  => ActorEntity
  -> m ()
updateViewshed e = do
  o <- getActor e
  p <- use #player
  tm <- use #tileMap
  let viewshed = o ^. #objectData % #viewshed
  let fov = calculateFov tm (o ^. objectPosition) (range viewshed)
  updateActor o (#objectData % #viewshed % #visibleTiles .~ fov)
  when (p == e) $
    let newlyRevealedTiles = map (,True) (S.toList fov)
    in #tileMap % #revealedTiles %= (//@ newlyRevealedTiles)

getVisibleTiles ::
  MonadStore Actor m
  => MonadState WorldState m
  => m (S.Set V2)
getVisibleTiles = do
  pl <- getPlayer
  return (pl ^. #objectData % #viewshed % #visibleTiles)

moveActorInDirection ::
  MonadState WorldState m
  => MonadStore Actor m
  => HasActorID a
  => a
  -> Direction
  -> m ()
moveActorInDirection e dir = do
  o <- getActor (actorID e)
  let newPos = calculateNewLocation dir $ o ^. objectPosition
  modifyObject o (objectPosition .~ newPos)
  makeViewshedDirty (actorID e)
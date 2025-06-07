{-# LANGUAGE RecordWildCards #-}
module Main where

import HsRogue.Prelude
import Data.List.NonEmpty

import BearLibTerminal
    ( terminalClear,
      terminalRefresh,
      terminalSet_
    )
import BearLibTerminal.Keycodes

import HsRogue.Map
import HsRogue.MapGen
import HsRogue.Renderable
import Rogue.Array2D.Boxed ( (!?@), traverseArrayWithCoord_ )
import Rogue.Colour ( terminalBkColour, terminalColour, black )
import Rogue.Config ( WindowOptions(..), defaultWindowOptions )
import Rogue.Events ( BlockingMode(..), handleEvents )
import Rogue.Geometry.Rectangle (centre)
import Rogue.Monad ( MonadRogue )
import Rogue.Rendering.Print ( printChar)
import Rogue.Window ( withWindow )
import qualified Data.Map as M
import HsRogue.World
import Rogue.Objects.Object (objectData)
import HsRogue.Object
import Rogue.Objects.Store
import Rogue.Objects.Entity

screenSize :: V2
screenSize = V2 100 50

initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

type GameMonad m = (MonadRogue m, MonadIO m, MonadState WorldState m)

main :: IO ()
main = do
  withWindow
    defaultWindowOptions { size = Just screenSize }
    initGame
    (evalStateT runLoop)
    (return ())

initGame :: MonadRogue m => m WorldState
initGame = do
  (madeMap, firstRoom:|_) <- roomsAndCorridorsMap 30 4 12 screenSize
  let addObjectsToWorld = do
        p <- addActor "player" playerRenderable (centre firstRoom)
        modify (\w -> w { player = p })
      initialWorld = (WorldState
        { tileMap = Tiles madeMap black
        , pendingQuit = False
        , actors = emptyStore
        , player = ActorEntity (Entity (-1))
        })
  execStateT addObjectsToWorld initialWorld

data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

movementKeys :: M.Map Keycode Direction
movementKeys = M.fromList
  [ (TkA, LeftDir)
  , (TkS, DownDir)
  , (TkW, UpDir)
  , (TkD, RightDir)
  , (TkUp, UpDir)
  , (TkDown, DownDir)
  , (TkLeft, LeftDir)
  , (TkRight, RightDir)
  ]

asMovement :: Keycode -> Maybe Direction
asMovement k = k `M.lookup` movementKeys

-- given a direction and a point, calculate the new point that is 1 tile in that direction.
calculateNewLocation :: Direction -> V2 -> V2
calculateNewLocation dir (V2 x y) = case dir of
  LeftDir -> V2 (x-1) y
  RightDir -> V2 (x+1) y
  UpDir -> V2 x (y-1)
  DownDir -> V2 x (y+1)

quitAfter :: MonadState WorldState m => m ()
quitAfter = modify (\worldState -> worldState { pendingQuit = True})

runLoop :: GameMonad m => m ()
runLoop = do
  terminalSet_ "font: KreativeSquare.ttf, size=16x16"
  terminalSet_ "output.vsync=false"
  terminalClear
  renderMap
  renderActors

  terminalRefresh
  _ <- handleEvents Blocking $ \case
    TkClose -> quitAfter
    TkEscape -> quitAfter
    other -> case asMovement other of
      Just dir -> do
        w <- get
        playerObject <- getPlayer
        let potentialNewLocation = calculateNewLocation dir (objectPosition playerObject)
            tileAtLocation = tiles (tileMap w) !?@ potentialNewLocation
        case tileAtLocation of
          Just t
            | walkable t -> updateActor playerObject (moveObject potentialNewLocation)
          _ -> return ()
      Nothing -> return ()
  shouldContinue <- not <$> gets pendingQuit
  when shouldContinue runLoop

renderMap :: GameMonad m => m ()
renderMap = do
  w <- get
  terminalBkColour (defaultBackgroundColour (tileMap w))
  traverseArrayWithCoord_ (tiles (tileMap w)) $ \p Tile{..} -> do
    terminalColour (foreground renderable)
    printChar p (glyph renderable)

renderActors :: GameMonad m => m ()
renderActors = do
  w <- get
  forM_ (actors w) $ \actor -> do
    terminalColour (foreground (objectRenderable actor))
    printChar (position . objectData $ actor) (glyph (objectRenderable actor))
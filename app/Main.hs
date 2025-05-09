{-# LANGUAGE RecordWildCards #-}
module Main where

import HsRogue.Prelude
import Data.List.NonEmpty

import BearLibTerminal
    ( terminalClear,
      terminalRefresh,
      textColor,
      terminalSet_
    )
import BearLibTerminal.Keycodes

import Rogue.Array2D.Boxed
import Rogue.Colour ( terminalBkColour, terminalColour, black )
import Rogue.Config ( WindowOptions(..), defaultWindowOptions )
import Rogue.Events ( BlockingMode(..), handleEvents )
import Rogue.Geometry.Rectangle (centre)
import Rogue.Rendering.Print (printText_, printChar)
import Rogue.Window ( withWindow )

import HsRogue.Map
import HsRogue.Renderable

import qualified Data.Map as M

screenSize :: V2
screenSize = V2 100 50

initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

type Game a = StateT WorldState IO a

data WorldState = WorldState
  { playerPosition :: V2
  , tileMap :: Tiles
  , pendingQuit :: Bool
  }

main :: IO ()
main = do
  withWindow
    defaultWindowOptions { size = Just screenSize }
    initGame
    (evalStateT runLoop)
    (return ())

initGame :: IO WorldState
initGame = do
  (madeMap, firstRoom:|_) <- roomsAndCorridorsMap 30 4 12 screenSize
  return (WorldState (centre firstRoom) (Tiles madeMap black) False)

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

runLoop :: Game ()
runLoop = do
  terminalSet_ "font: KreativeSquare.ttf, size=24x24"
  terminalClear
  playerPos <- gets playerPosition
  renderMap
  printText_ playerPos (textColor "white" "@")
  terminalRefresh
  _ <- handleEvents Blocking $ \case
    TkClose -> modify (\worldState -> worldState { pendingQuit = True})
    TkEscape -> modify (\worldState -> worldState { pendingQuit = True})
    other -> case asMovement other of
      Just dir -> do
        w <- get
        let potentialNewLocation = calculateNewLocation dir (playerPosition w)
            tileAtLocation = tiles (tileMap w) !?@ potentialNewLocation
        case tileAtLocation of
          Just t
            | walkable t ->  modify (\worldState -> worldState { playerPosition = potentialNewLocation })
          _ -> return ()
      Nothing -> return ()
  shouldContinue <- not <$> gets pendingQuit
  when shouldContinue runLoop


renderMap :: Game ()
renderMap = do
  w <- get
  let es = tileMap w
  terminalBkColour (defaultBackgroundColour es)
  traverseArrayWithCoord_ (tiles es) $ \p Tile{..} -> do
    terminalColour (foreground renderable)
    printChar p (glyph renderable)
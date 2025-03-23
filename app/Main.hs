{-# LANGUAGE RecordWildCards #-}
module Main where

import HsRogue.Prelude

import BearLibTerminal ( terminalClear, terminalRefresh )
import BearLibTerminal.Keycodes

import Rogue.Config ( WindowOptions(..), defaultWindowOptions )
import Rogue.Events ( BlockingMode(..), handleEvents )
import Rogue.Rendering.Print (printText)
import Rogue.Window ( withWindow )

import qualified Data.Map as M
import BearLibTerminal
import Rogue.Array2D.Boxed
import Rogue.Colour
import HsRogue.Map
import Data.List.NonEmpty
import Rogue.Geometry.Rectangle (centre)

screenSize :: V2
screenSize = V2 100 50

initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

type Game a = StateT WorldState IO a

data WorldState = WorldState
  { playerPosition :: V2
  , tileMap :: Array2D Tile
  , pendingQuit :: Bool
  }

playerRenderable :: Renderable
playerRenderable = Renderable '@' (fromRGB 0x75 0xa2 0xeb) (Colour 0x00000000)

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
  return (WorldState (centre firstRoom) madeMap False)

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

calculateNewLocation :: Direction -> V2 -> V2
calculateNewLocation dir v =
  let updateIt :: V2 -> V2
      updateIt = case dir of
        LeftDir -> modifyX (subtract 1)
        RightDir -> modifyX (+ 1)
        UpDir -> modifyY (subtract 1)
        DownDir -> modifyY (+ 1)
  in
    updateIt v

runLoop :: Game ()
runLoop = do
  terminalSet_ "font: KreativeSquare.ttf, size=24x24"
  terminalClear
  playerPos <- gets playerPosition
  renderMap
  withV2 playerPos terminalPrint_ (textColor "white" "@")
  terminalRefresh
  _ <- handleEvents Blocking $ \case
    TkClose -> modify (\worldState -> worldState { pendingQuit = True})
    TkEscape -> modify (\worldState -> worldState { pendingQuit = True})
    other -> case asMovement other of
      Just dir -> do
        w <- get
        let potentialNewLocation = calculateNewLocation dir (playerPosition w)
            tileAtLocation = (tileMap w) !?@ potentialNewLocation
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
  traverseArrayWithCoord_ es $ \p Tile{..} -> do
    terminalColour (foreground renderable)
    terminalBkColour (background renderable)
    withV2 p terminalPut (glyph renderable)
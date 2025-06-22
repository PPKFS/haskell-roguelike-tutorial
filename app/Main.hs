module Main where

import HsRogue.Prelude

import BearLibTerminal ( terminalClear, terminalRefresh )
import BearLibTerminal.Keycodes
import Rogue.Config ( WindowOptions(..), defaultWindowOptions )
import Rogue.Events ( BlockingMode(..), handleEvents_ )
import Rogue.Rendering.Print  ( printText_ )
import Rogue.Window ( withWindow )

import qualified Data.Map as M

screenSize :: V2
screenSize = V2 100 50

initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

type GameMonad m = (MonadIO m, MonadState WorldState m)

data WorldState = WorldState
  { playerPosition :: V2
  , pendingQuit :: Bool
  }

main :: IO ()
main =
  withWindow
    defaultWindowOptions { size = Just screenSize, title = Just "HsRogue - Part 1" }
    (return ()) -- no init logic
    (const $ evalStateT runLoop (WorldState initialPlayerPosition False))
    (return ()) -- no shutdown logic

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
calculateNewLocation dir (V2 x y) = case dir of
  LeftDir -> V2 (x-1) y
  RightDir -> V2 (x+1) y
  UpDir -> V2 x (y-1)
  DownDir -> V2 x (y+1)

pendQuit :: GameMonad m => m ()
pendQuit = modify (\worldState -> worldState { pendingQuit = True})

runLoop :: GameMonad m => m ()
runLoop = do
  terminalClear
  playerPos <- gets playerPosition
  printText_ playerPos "@"
  terminalRefresh
  handleEvents_ Blocking $ \case
    TkClose -> pendQuit
    TkEscape -> pendQuit
    other -> case asMovement other of
      Just dir -> modify (\worldState ->
        worldState
          { playerPosition = calculateNewLocation dir (playerPosition worldState)
          })
      Nothing -> return ()
  shouldQuit <- gets pendingQuit
  unless shouldQuit runLoop
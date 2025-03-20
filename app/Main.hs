module Main where

import HsRogue.Prelude

import BearLibTerminal ( terminalClear, terminalRefresh )
import BearLibTerminal.Keycodes

import Rogue.Config ( WindowOptions(..), defaultWindowOptions )
import Rogue.Events ( BlockingMode(..), handleEvents )
import Rogue.Rendering.Print (printText)
import Rogue.Window ( withWindow )

import qualified Data.Map as M

screenSize :: V2
screenSize = V2 100 50

initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

type Game a = StateT WorldState IO a

data WorldState = WorldState
  { playerPosition :: V2
  , pendingQuit :: Bool
  }

main :: IO ()
main = flip evalStateT (WorldState initialPlayerPosition False) $ do
  withWindow
    defaultWindowOptions { size = Just screenSize }
    (return ())
    (\_init -> runLoop)
    (return ())

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
  terminalClear
  playerPos <- gets playerPosition
  _ <- withV2 playerPos terminalPrint "@"
  terminalRefresh
  _ <- handleEvents Blocking $ \case
    TkClose -> modify (\worldState -> worldState { pendingQuit = True})
    TkEscape -> modify (\worldState -> worldState { pendingQuit = True})
    other -> case asMovement other of
      Just dir -> modify (\worldState ->
        worldState
          { playerPosition = calculateNewLocation dir (playerPosition worldState)
          })
      Nothing -> return ()
  shouldContinue <- not <$> gets pendingQuit
  when shouldContinue runLoop
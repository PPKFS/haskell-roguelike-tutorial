module Main where

import Prelude -- we'll make our own prelude in part 1

import BearLibTerminal
import BearLibTerminal.Keycodes
import Control.Monad (when)
import Rogue.Config
import Rogue.Events
import Rogue.Geometry.V2
import Rogue.Window

screenSize :: V2
screenSize = V2 100 50

main :: IO ()
main =
  withWindow
  defaultWindowOptions { size = Just screenSize }
  (return ()) -- no init logic
  (const runLoop)
  (return ()) -- no shutdown logic

runLoop :: IO ()
runLoop = do
  terminalRefresh
  -- event handling
  shouldContinue <- handleEvents Blocking $ \case
    TkResized -> return True
    TkClose -> return False
    TkEscape -> return False
    _ -> return False
  when (and shouldContinue) runLoop
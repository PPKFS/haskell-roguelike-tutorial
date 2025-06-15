module Main where

import Prelude -- we'll make our own prelude in part 1

import BearLibTerminal ( terminalRefresh )
-- note that pattern synonyms don't like manual imports
import BearLibTerminal.Keycodes

import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO(..) )

import Rogue.Config ( WindowOptions(..), defaultWindowOptions )
import Rogue.Events ( BlockingMode(..), handleEvents )
import Rogue.Geometry.V2 ( V2(..) )
import Rogue.Window ( withWindow )

screenSize :: V2
screenSize = V2 100 50

main :: IO ()
main =
  withWindow
  defaultWindowOptions { size = Just screenSize, title = Just "HsRogue - Part 0" }
  (return ()) -- no init logic
  (const $ liftIO runLoop)
  (return ()) -- no shutdown logic

runLoop :: IO ()
runLoop = do
  terminalRefresh
  -- event handling
  shouldContinue <- handleEvents Blocking $ \case
    TkClose -> return False
    TkEscape -> return False
    _ -> return True
  when (and shouldContinue) runLoop
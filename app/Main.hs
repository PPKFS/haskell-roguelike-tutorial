module Main where

import HsRogue.Prelude
import Data.List.NonEmpty ( NonEmpty(..) )

import BearLibTerminal
    ( terminalClear,
      terminalRefresh,
      terminalSet_
    )

import BearLibTerminal.Keycodes

import Optics.State.Operators ((.=))

import Rogue.Array2D.Boxed ( (!?@), traverseArrayWithCoord_, replicateArray )
import Rogue.Colour ( terminalColour, black, desaturate, toGreyscale )
import Rogue.Config ( WindowOptions(..), defaultWindowOptions )
import Rogue.Events ( BlockingMode(..), handleEvents_ )
import Rogue.Geometry.Rectangle ( centre )
import Rogue.Monad ( MonadRogue, MonadStore )
import Rogue.Objects.Entity ( Entity(..) )
import Rogue.Objects.Store ( emptyStore )
import Rogue.Rendering.Print ( printChar)
import Rogue.Tilemap ( MonadTiles(..) )
import Rogue.Window ( withWindow )

import HsRogue.Map
import HsRogue.MapGen
import HsRogue.Object
import HsRogue.Renderable
import HsRogue.Viewshed
import HsRogue.World

import qualified Data.Map as M

screenSize :: V2
screenSize = V2 100 50

initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

type GameMonad m = (MonadTiles Tile m, MonadRogue m, MonadIO m, MonadState WorldState m, MonadStore Actor m)

main :: IO ()
main = do
  withWindow
    defaultWindowOptions { size = Just screenSize, title = Just "HsRogue - Part 3b" }
    initGame
    (evalStateT runLoop)
    (return ())

initGame :: (MonadIO m, MonadRogue m) => m WorldState
initGame = do
  terminalSet_ "font: KreativeSquare.ttf, size=16x16"
  (madeMap, firstRoom:|_) <- roomsAndCorridorsMap 30 4 12 screenSize
  let addObjectsToWorld = do
        p <- addActor "player" playerRenderable (centre firstRoom) 20
        #player .= p
        makeAllViewshedsDirty
        updateViewsheds
      initialWorld = (WorldState
        { tileMap = Tiles
          { tiles = madeMap
          , defaultBackgroundColour = black
          , revealedTiles = replicateArray False screenSize
          }
        , pendingQuit = False
        , actors = emptyStore
        , player = ActorEntity (Entity (-1))
        , dirtyViewsheds = []
        })
  execStateT addObjectsToWorld initialWorld

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

pendQuit :: MonadState WorldState m => m ()
pendQuit = #pendingQuit .= True

getTileAtLocation :: MonadState WorldState m => V2 -> m (Maybe Tile)
getTileAtLocation loc = use $ #tileMap % #tiles % to (!?@ loc)

runLoop :: GameMonad m => m ()
runLoop = do
  everyTurn
  terminalClear
  renderMap
  renderActors
  terminalRefresh
  handleEvents_ Blocking $ \case
    TkClose -> pendQuit
    TkEscape -> pendQuit
    other -> case asMovement other of
      Just dir -> do
        playerObject <- getPlayer
        let potentialNewLocation = calculateNewLocation dir (playerObject ^. objectPosition)
        tileAtLocation <- getTileAtLocation potentialNewLocation
        case tileAtLocation of
          Just t
            | walkable t -> moveActorInDirection playerObject dir
          _ -> return ()
      Nothing -> return ()
  shouldQuit <- gets pendingQuit
  unless shouldQuit runLoop

renderMap :: GameMonad m => m ()
renderMap = do
  w <- get
  let es = w ^. #tileMap
  traverseArrayWithCoord_ (es ^. #revealedTiles) $ \p rev -> when rev $ do
    t <- getTileM @Tile p
    let r = t ^. #renderable
    terminalColour (desaturate $ toGreyscale $ r ^. #foreground)
    printChar p (r ^. #glyph)
  playerViewshed <- getVisibleTiles
  forM_ playerViewshed $ \p -> do
    t <- getTileM @Tile p
    let r = t ^. #renderable
    terminalColour (r ^. #foreground)
    printChar p (r ^. #glyph)

renderActors :: GameMonad m => m ()
renderActors = do
  actors <- use #actors
  forM_ actors $ \actor -> do
    let r = actor ^. objectRenderable
    terminalColour (foreground r)
    printChar (actor ^. objectPosition) (glyph r)

everyTurn :: GameMonad m => m ()
everyTurn = do
  updateViewsheds

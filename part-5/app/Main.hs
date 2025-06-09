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
import HsRogue.Object
import HsRogue.Renderable
import HsRogue.World

import HsRogue.Viewshed

import Optics ( to, (%), (^.), use, At (..), (^?), view )
import Optics.State.Operators ((.=))

import Rogue.Array2D.Boxed ( (!?@), traverseArrayWithCoord_, replicateArray )
import Rogue.Colour ( terminalColour, black, desaturate, toGreyscale )
import Rogue.Config ( WindowOptions(..), defaultWindowOptions )
import Rogue.Events ( BlockingMode(..), handleEvents )
import Rogue.Geometry.Rectangle (centre)
import Rogue.Monad ( MonadRogue, MonadStore )
import Rogue.Objects.Entity ( Entity(..) )
import Rogue.Objects.Store ( emptyStore )
import Rogue.Rendering.Print ( printChar)
import Rogue.Tilemap
import Rogue.Window ( withWindow )
import qualified Data.Map as M
import Rogue.FieldOfView.Visibility
import Rogue.AStar (findPath)
import Rogue.Random
import qualified Data.Text as T

screenSize :: V2
screenSize = V2 100 50

initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

type GameMonad m = (MonadTiles Tile m, MonadRogue m, MonadIO m, MonadState WorldState m, MonadStore Actor m)

main :: IO ()
main = do
  withWindow
    defaultWindowOptions { size = Just screenSize }
    initGame
    (evalStateT runLoop)
    (return ())

initGame :: (MonadIO m, MonadRogue m) => m WorldState
initGame = do
  terminalSet_ "font: KreativeSquare.ttf, size=16x16"
  (madeMap, firstRoom:|otherRooms) <- roomsAndCorridorsMap 30 4 12 screenSize
  let initialWorld = (WorldState
        { tileMap = Tiles
          { tiles = madeMap
          , defaultBackgroundColour = black
          , revealedTiles = replicateArray False screenSize
          , tileContents = M.empty
          }
        , pendingQuit = False
        , actors = emptyStore
        , player = ActorEntity (Entity (-1))
        , dirtyViewsheds = []
        })
      addObjectsToWorld = do
        p <- addActor playerKind "player" playerRenderable (centre firstRoom) 20 (PlayerSpecifics ())
        #player .= p
        enumerateFromM_ 1 otherRooms $ \i room -> do
          let centreOfRoom = centre room
              goblinName = "Goblin #" <> showText i
          randomBehaviour <- randomEnum
          randomInsult <- choose
            [ "that your mother smells of elderberries"
            , "that your father was a hamster"
            , "\"I fart in your general direction\""
            ]
          addActor monsterKind goblinName goblinRenderable centreOfRoom 5
            (MonsterS $ MonsterSpecifics randomInsult randomBehaviour False)
        makeAllViewshedsDirty
        updateViewsheds


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

quitAfter :: MonadState WorldState m => m ()
quitAfter = #pendingQuit .= True

runLoop :: GameMonad m => m ()
runLoop = do
  everyTurn
  terminalClear
  renderMap
  renderActors
  terminalRefresh
  _ <- handleEvents Blocking $ \case
    TkClose -> quitAfter
    TkEscape -> quitAfter
    other -> case asMovement other of
      Just dir -> do
        playerObject <- getPlayer
        tm <- use #tileMap
        let potentialNewLocation = calculateNewLocation dir (playerObject ^. objectPosition)
            canWalkOnTile = positionAllowsMovement tm potentialNewLocation
        when canWalkOnTile $ moveActorInDirection playerObject dir
      Nothing -> return ()
  shouldContinue <- not <$> gets pendingQuit
  when shouldContinue runLoop

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
  tiles <- getVisibleTiles
  forM_ actors $ \actor -> when (actor ^. objectPosition `elem` tiles) $ do
    let r = actor ^. objectRenderable
    terminalColour (foreground r)
    printChar (actor ^. objectPosition) (glyph r)

monstersThink :: GameMonad m => m ()
monstersThink = do
  actors <- use #actors
  playerLocation <- view objectPosition <$> getPlayer
  forM_ actors $ \actor ->
    whenJust (actor ^? #specifics % #_MonsterS) $ \monsterStuff ->
    when (playerLocation `elem` actor ^. #objectData % #viewshed % #visibleTiles) $ do
      insultPlayer (actor ^. #name) (insult monsterStuff)
      case behaviour monsterStuff of
        AttackPlayer -> do
          m <- use #tileMap
          r <- findPath m (actor ^. objectPosition) playerLocation
          case r of
            Just (nextStep:_:_) ->
              when (positionAllowsMovement m nextStep) $
                moveActor actor nextStep
            _ -> return ()
          return ()
        FleeFromPlayer -> return ()
      return ()

insultPlayer :: MonadIO m => Text -> Text -> m ()
insultPlayer name insult = liftIO $ putStrLn $ T.unpack $ name <> " yells " <> insult  <> "!"

everyTurn :: GameMonad m => m ()
everyTurn = do
  monstersThink
  updateViewsheds
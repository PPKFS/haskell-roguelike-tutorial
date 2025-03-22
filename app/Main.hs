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
import Rogue.Geometry.Rectangle
import System.Random.Stateful

screenSize :: V2
screenSize = V2 100 50

-- | We want to keep some sort of fixed set of tiles with their relevant properties.
data TileType = Floor | Wall
  deriving stock (Eq, Ord, Show, Generic)

-- Somethign that represents a renderable character. Just one.
data Renderable = Renderable
  { glyph :: Char
  , foreground :: Colour
  , background :: Colour
  } deriving stock (Show, Read, Generic)

-- because of immutability we don't need to worry about these being heavyweight or whatever.
data Tile = Tile
  { name :: Text
  , renderable :: Renderable
  , walkable :: Bool
  } deriving stock (Generic, Show)

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

floorTile :: Tile
floorTile = Tile "floor" (Renderable '.' (Colour 0xFF008888) (Colour 0x00000000)) True

wallTile :: Tile
wallTile = Tile "wall" (Renderable '#' (Colour 0xFF00FF00) (Colour 0x00000000)) False

emptyMap :: Int -> V2 -> IO (Array2D Tile)
emptyMap numberOfWalls size = do
  let floorMap = replicateArray floorTile size
  randomWalls <- mapM (const $ uniformRM (V2 0 0, size - V2 1 1) globalStdGen) [1..numberOfWalls]
  return $ floorMap //@ map (, wallTile) ((rectangleEdges (rectangleFromDimensions (V2 0 0) screenSize)) <> randomWalls)

main :: IO ()
main = do
  withWindow
    defaultWindowOptions { size = Just screenSize }
    initGame
    (evalStateT runLoop)
    (return ())

initGame :: IO WorldState
initGame = do
  let (worldRandomProportion :: Double) = 0.2
  madeMap <- emptyMap (round $ worldRandomProportion * fromIntegral (v2AsArea screenSize)) screenSize
  return (WorldState initialPlayerPosition madeMap False)

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
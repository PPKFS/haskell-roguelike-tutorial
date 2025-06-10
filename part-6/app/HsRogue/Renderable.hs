module HsRogue.Renderable
  ( Renderable(..)
  , playerRenderable
  , floorRenderable
  , wallRenderable
  , goblinRenderable
  ) where

import HsRogue.Prelude
import Rogue.Colour ( ivory, lightSlateGray, mediumSeaGreen, Colour, oliveDrab )

-- Somethign that represents a renderable character. Just one.
data Renderable = Renderable
  { glyph :: Char
  , foreground :: Colour
  } deriving (Show, Read, Ord, Eq, Generic)

playerRenderable :: Renderable
playerRenderable = Renderable '@' ivory

floorRenderable :: Renderable
floorRenderable = Renderable '.' lightSlateGray

wallRenderable :: Renderable
wallRenderable = Renderable '#' mediumSeaGreen

goblinRenderable :: Renderable
goblinRenderable = Renderable 'g' oliveDrab
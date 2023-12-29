module Went.Geometry.Spot where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Spot
  = Spot
      { x :: Number
      , y :: Number
      , offsetx :: Number
      , offsety :: Number
      }
  | TopSide
  | TopBottomSides
  | TopLeftSides
  | TopRightSides
  | BottomSide
  | BottomLeftSides
  | BottomRightSides
  | LeftRightSides
  | LeftSide
  | RightSide

derive instance Generic Spot _
instance Show Spot where
  show = genericShow

top :: Spot
top = Spot { x: 0.5, y: 0.0, offsetx: 0.0, offsety: 0.0 }

topRight :: Spot
topRight = Spot { x: 1.0, y: 0.0, offsetx: 0.0, offsety: 0.0 }

topLeft :: Spot
topLeft = Spot { x: 0.0, y: 0.0, offsetx: 0.0, offsety: 0.0 }

bottom :: Spot
bottom = Spot { x: 0.5, y: 1.0, offsetx: 0.0, offsety: 0.0 }

bottomRight :: Spot
bottomRight = Spot { x: 1.0, y: 1.0, offsetx: 0.0, offsety: 0.0 }

bottomLeft :: Spot
bottomLeft = Spot { x: 0.0, y: 1.0, offsetx: 0.0, offsety: 0.0 }

left :: Spot
left = Spot { x: 0.0, y: 0.5, offsetx: 0.0, offsety: 0.0 }

right :: Spot
right = Spot { x: 1.0, y: 0.5, offsetx: 0.0, offsety: 0.0 }

center :: Spot
center = Spot { x: 0.5, y: 0.5, offsetx: 0.0, offsety: 0.0 }
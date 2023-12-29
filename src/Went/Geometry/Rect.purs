module Went.Geometry.Rect where

data Rect
  = RectEach
      { x :: Number
      , y :: Number
      , w :: Number
      , h :: Number
      }
  | RectAll Number

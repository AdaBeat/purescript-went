module Went.GraphObject.Shape.StrokeJoin where

import Prelude

data StrokeJoin 
  = Round
  | Bevel
  | Miter
instance Show StrokeJoin where
  show = case _ of
    Round -> "round"
    Bevel -> "bevel"
    Miter -> "miter"
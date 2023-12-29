module Went.GraphObject.Shape.StrokeCap where

import Prelude

data StrokeCap
  = Butt
  | Round
  | Square
instance Show StrokeCap where
  show = case _ of
    Butt -> "butt"
    Round -> "round"
    Square -> "square"
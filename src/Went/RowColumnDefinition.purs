module Went.RowColumnDefinition where

import Went.Geometry.Margin (Margin)
import Went.Geometry.Spot (Spot)
import Went.GraphObject.EnumValue.Stretch (Stretch)
import Went.RowColumnDefinition.EnumValue.Sizing (Sizing)

type RowColumnDefinitionFields =
  ( actual :: Number
  , alignment :: Spot
  , background :: String
  , coversSeparators :: Boolean
  , height :: Number
  , index :: Int
  , isRow :: Boolean
  , minimum :: Number
  , separatorDashArray :: Array Number
  , separatorPadding :: Margin
  , separatorStroke :: String
  , separatorStrokeWidth :: Number
  , sizing :: Sizing
  , stretch :: Stretch
  , width :: Number
  )
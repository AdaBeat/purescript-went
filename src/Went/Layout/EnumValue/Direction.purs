module Went.Layout.EnumValue.Direction where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Direction
  = Clockwise
  | Counterclokwise
  | BidirectionalLeft
  | BidirectionalRight

derive instance Generic Direction _
instance Show Direction where
  show = genericShow

instance EnumValue "CircularLayout" Direction

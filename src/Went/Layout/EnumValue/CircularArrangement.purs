module Went.Layout.EnumValue.CircularArrangement where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Arrangement
  = ConstantSpacing
  | ConstantAngle
  | ConstantDistance
  | Packed

derive instance Generic Arrangement _
instance Show Arrangement where
  show = genericShow

instance EnumValue "CircularLayout" Arrangement

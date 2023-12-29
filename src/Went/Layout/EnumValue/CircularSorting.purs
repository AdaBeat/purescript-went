module Went.Layout.EnumValue.CircularSorting where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Sorting
  = Optimized
  | Ascending
  | Descending
  | Forwards
  | Reverse

derive instance Generic Sorting _
instance Show Sorting where
  show = genericShow

instance EnumValue "CircularLayout" Sorting

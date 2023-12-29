module Went.Layout.EnumValue.GridSorting where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Sorting
  = Ascending
  | Descending
  | Forward
  | Reverse

derive instance Generic Sorting _
instance Show Sorting where
  show = genericShow

instance EnumValue "GridLayout" Sorting

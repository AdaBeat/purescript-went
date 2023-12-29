module Went.Layout.EnumValue.TreeArrangement where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Arrangement
  = ArrangementVertical
  | ArrangementHorizontal
  | ArrangementFixedRoots

derive instance Generic Arrangement _
instance Show Arrangement where
  show = genericShow

instance EnumValue "TreeLayout" Arrangement

module Went.Layout.EnumValue.GridArrangement where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Arrangement
  = LeftToRight -- default
  | RightToLeft

derive instance Generic Arrangement _
instance Show Arrangement where
  show = genericShow

instance EnumValue "GridLayout" Arrangement

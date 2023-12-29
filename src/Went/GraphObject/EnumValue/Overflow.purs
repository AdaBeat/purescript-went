module Went.GraphObject.EnumValue.Overflow where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Overflow
  = OverflowClip
  | OverflowEllipsis
derive instance Generic Overflow _
instance Show Overflow where
  show = genericShow

instance EnumValue "TextBlock" Overflow

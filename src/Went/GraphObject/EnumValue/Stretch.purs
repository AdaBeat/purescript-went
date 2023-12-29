module Went.GraphObject.EnumValue.Stretch where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Stretch
  = None
  | Fill
  | Horizontal
  | Vertical

derive instance Generic Stretch _
instance Show Stretch where
  show = genericShow

instance EnumValue "GraphObject" Stretch

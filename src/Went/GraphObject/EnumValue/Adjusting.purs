module Went.GraphObject.EnumValue.Adjusting where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Adjusting
  = None
  | End
  | Scale
  | Stretch

derive instance Generic Adjusting _
instance Show Adjusting where
  show = genericShow

instance EnumValue "Link" Adjusting

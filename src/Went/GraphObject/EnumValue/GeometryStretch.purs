module Went.GraphObject.EnumValue.GeometryStretch where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data GeometryStretch
  = Default
  | None
  | Fill
  | Uniform

derive instance Generic GeometryStretch _
instance Show GeometryStretch where
  show = genericShow

instance EnumValue "GraphObject" GeometryStretch

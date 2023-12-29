module Went.Diagram.EnumValue.AutoScale where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data AutoScale
  = None
  | Uniform
  | UniformToFill

derive instance Generic AutoScale _
instance Show AutoScale where
  show = genericShow

instance EnumValue "GraphObject" AutoScale


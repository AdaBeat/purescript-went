module Went.GraphObject.EnumValue.ViewboxStretch where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data ViewboxStretch
  = Uniform
  | UniformToFill

derive instance Generic ViewboxStretch _
instance Show ViewboxStretch where
  show = genericShow

instance EnumValue "GraphObject" ViewboxStretch

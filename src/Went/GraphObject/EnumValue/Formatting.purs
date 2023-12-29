module Went.GraphObject.EnumValue.Formatting where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Formatting
  = FormatTrim
  | FormatNone
derive instance Generic Formatting _
instance Show Formatting where
  show = genericShow

instance EnumValue "TextBlock" Formatting

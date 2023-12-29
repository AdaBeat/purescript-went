module Went.RowColumnDefinition.EnumValue.Sizing where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Sizing
  = Default
  | None
  | ProportionalExtra

derive instance Generic Sizing _
instance Show Sizing where
  show = genericShow

instance EnumValue "RowColumnDefinition" Sizing

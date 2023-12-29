module Went.GraphObject.EnumValue.Wrap where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Wrap
  = WrapFit
  | WrapDesiredSize
  | WrapBreakAll

derive instance Generic Wrap _
instance Show Wrap where
  show = genericShow

instance EnumValue "TextBlock" Wrap

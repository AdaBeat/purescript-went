module Went.GraphObject.EnumValue.Curve where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Curve
  = None
  | End
  | Bezier
  | JumpGap
  | JumpOver

derive instance Generic Curve _
instance Show Curve where
  show = genericShow

instance EnumValue "Link" Curve

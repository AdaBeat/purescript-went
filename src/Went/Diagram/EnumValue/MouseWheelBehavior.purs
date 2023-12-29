module Went.Diagram.EnumValue.MouseWheelBehavior where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data MouseWheelBehavior
  = WheelScroll
  | WheelZoom
  | WheelNone

derive instance Generic MouseWheelBehavior _
instance Show MouseWheelBehavior where
  show = genericShow

instance EnumValue "ToolManager" MouseWheelBehavior


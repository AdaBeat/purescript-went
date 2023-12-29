module Went.Diagram.EnumValue.GestureBehavior where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data GestureBehavior
  = GestureZoom
  | GestureCancel
  | GestureNone

derive instance Generic GestureBehavior _
instance Show GestureBehavior where
  show = genericShow

instance EnumValue "ToolManager" GestureBehavior

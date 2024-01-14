module Went.Diagram.Animation.EnumValue.StartCondition where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data StartCondition
  = Default
  | Bundled
  | Immediate

derive instance Generic StartCondition _
instance Show StartCondition where
  show = genericShow

instance EnumValue "AnimationTrigger" StartCondition

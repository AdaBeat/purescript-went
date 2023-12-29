module Went.Diagram.EnumValue.InitialAnimationStyle where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data InitialAnimationStyle
  = None
  | Default
  | AnimateLocations

derive instance Generic InitialAnimationStyle _
instance Show InitialAnimationStyle where
  show = genericShow

instance EnumValue "AnimationManager" InitialAnimationStyle


module Went.GraphObject.EnumValue.Flip where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Flip
  = None
  | FlipBoth
  | FlipHorizontal
  | FlipVertical

derive instance Generic Flip _
instance Show Flip where
  show = genericShow

instance EnumValue "GraphObject" Flip

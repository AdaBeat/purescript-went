module Went.Layout.EnumValue.LayeringOption where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data LayeringOption
  = LayerLongestPathSink
  | LayerLongestPathSource
  | LayerOptimalLinkLength

derive instance Generic LayeringOption _
instance Show LayeringOption where
  show = genericShow

instance EnumValue "LayeredDigraphLayout" LayeringOption

module Went.Layout.EnumValue.TreeStyle where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data TreeStyle
  = StyleLayered
  | StyleAlternating
  | StyleLastParents
  | StyleRootOnly

derive instance Generic TreeStyle _
instance Show TreeStyle where
  show = genericShow

instance EnumValue "TreeLayout" TreeStyle

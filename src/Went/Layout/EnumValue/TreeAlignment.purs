module Went.Layout.EnumValue.TreeAlignment where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Alignment
  = AlignmentBottomRightBus
  | AlignmentBus
  | AlignmentBusBranching
  | AlignmentCenterChildren
  | AlignmentCenterSubtrees
  | AlignmentEnd
  | AlignmentStart
  | AlignmentTopLeftBus

derive instance Generic Alignment _
instance Show Alignment where
  show = genericShow

instance EnumValue "TreeLayout" Alignment

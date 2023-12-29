module Went.GraphObject.EnumValue.SegmentOrientation where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data SegmentOrientation
  = OrientAlong
  | OrientMinus90
  | OrientMinus90Upright
  | OrientOpposite
  | OrientPlus90
  | OrientPlus90Upright
  | OrientUpright
  | OrientUpright45

derive instance Generic SegmentOrientation _
instance Show SegmentOrientation where
  show = genericShow

instance EnumValue "Link" SegmentOrientation

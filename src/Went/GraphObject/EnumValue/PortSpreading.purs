module Went.GraphObject.EnumValue.PortSpreading where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data PortSpreading
  = SpreadingNone
  | SpreadingEvenly
  | SpreadingPacked

derive instance Generic PortSpreading _
instance Show PortSpreading where
  show = genericShow

instance EnumValue "Node" PortSpreading

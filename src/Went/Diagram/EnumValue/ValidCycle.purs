module Went.Diagram.EnumValue.ValidCycle where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data ValidCycle
  = CycleAll
  | CycleDestinationTree
  | CycleNotDirected
  | CycleNotUndirected
  | CycleSourceTree

derive instance Generic ValidCycle _
instance Show ValidCycle where
  show = genericShow

instance EnumValue "GraphObject" ValidCycle

module Went.GraphObject.EnumValue.Routing where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Routing
  = Normal
  | Orthogonal
  | AvoidsNodes

derive instance Generic Routing _
instance Show Routing where
  show = genericShow

instance EnumValue "Link" Routing

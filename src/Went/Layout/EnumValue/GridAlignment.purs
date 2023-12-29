module Went.Layout.EnumValue.GridAlignment where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data Alignment
  = Location -- default
  | Position

derive instance Generic Alignment _
instance Show Alignment where
  show = genericShow

instance EnumValue "GridLayout" Alignment

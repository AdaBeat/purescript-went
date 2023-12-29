module Went.Layout.EnumValue.NodeDiameterFormula where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Went.EnumValue (class EnumValue)

data NodeDiameterFormula
  = Pythagorean
  | Circular

derive instance Generic NodeDiameterFormula _
instance Show NodeDiameterFormula where
  show = genericShow

instance EnumValue "CircularLayout" NodeDiameterFormula

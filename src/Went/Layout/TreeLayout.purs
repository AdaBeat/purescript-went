module Went.Layout.TreeLayout where

import GoJS.Layout.Types (TreeLayout_, TreeNetwork_)
import Went.Layout (LayoutSpecificFields)
import Went.Layout.EnumValue.TreeAlignment (Alignment)
import Went.Layout.EnumValue.TreeArrangement (Arrangement)
import Went.Layout.EnumValue.TreeStyle (TreeStyle)

type TreeSpecificFields =
  ( treeStyle :: TreeStyle
  , arrangement :: Arrangement
  , layerSpacing :: Number
  , nodeSpacing :: Number
  , alternateAngle :: Number
  , alternateLayerSpacing :: Number
  , alternateAlignment :: Alignment
  , alternateNodeSpacing :: Number
  , angle :: Number
  )

type TreeFields = LayoutSpecificFields TreeLayout_ TreeNetwork_ TreeSpecificFields

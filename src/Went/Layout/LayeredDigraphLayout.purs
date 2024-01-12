module Went.Layout.LayeredDigraphLayout where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import GoJS.Diagram.Layout.Constants (alignAll_, alignLowerLeft_, alignLowerRight_, alignNone_, alignUpperLeft_, alignUpperRight_, packAll_, packExpand_, packMedian_, packNone_, packStraighten_)
import GoJS.Layout (LayeredDigraphLayout_, LayeredDigraphVertex_)
import GoJS.Layout.Types (LayeredDigraphNetwork_)
import Went.FFI.Override (Override)
import Went.Layout (LayoutSpecificFields)
import Went.Layout.EnumValue.LayeringOption (LayeringOption)

-- These option types and their semigroup instances are here to emulate the Sankey's use of || in
-- this way: go.LayeredDigraphLayout.PackStraighten || go.LayeredDigraphLayout.PackMedian. I don't know
-- why that flavor of flag-setting makes any sense in gojs (it's just evaluating to the first nonzero of those,
-- and the library obviously knows what it is) but I'm just emulating the behavior. These numbers
-- are wrapped in newtypes so that only the newtyped constants are usable - they're essentially smart constructors.
newtype PackOption = PackOption Number

packNone :: PackOption
packNone = PackOption packNone_

packExpand :: PackOption
packExpand = PackOption packExpand_

packStraighten :: PackOption
packStraighten = PackOption packStraighten_

packMedian :: PackOption
packMedian = PackOption packMedian_

packAll :: PackOption
packAll = PackOption packAll_

instance Semigroup PackOption where
  append (PackOption p1) (PackOption p2) = PackOption $ if p1 == 0.0 then p2 else p1

derive instance Newtype PackOption _

newtype AlignOption = AlignOption Number

alignNone :: AlignOption
alignNone = AlignOption alignNone_

alignUpperLeft :: AlignOption
alignUpperLeft = AlignOption alignUpperLeft_

alignUpperRight :: AlignOption
alignUpperRight = AlignOption alignUpperRight_

alignLowerLeft :: AlignOption
alignLowerLeft = AlignOption alignLowerLeft_

alignLowerRight :: AlignOption
alignLowerRight = AlignOption alignLowerRight_

alignAll :: AlignOption
alignAll = AlignOption alignAll_

instance Semigroup AlignOption where
  append (AlignOption p1) (AlignOption p2) = AlignOption $ if p1 == 0.0 then p2 else p1

derive instance Newtype AlignOption _

type LayeredDigraphSpecificFields =
  ( alignOption :: AlignOption
  , setsPortSpots :: Boolean
  , direction :: Number
  , layeringOption :: LayeringOption
  , packOption :: PackOption
  , layerSpacing :: Number
  , columnSpacing :: Number
  , nodeMinColumnSpace :: Override LayeredDigraphLayout_ (LayeredDigraphVertex_ -> Boolean -> Effect Number)
  , nodeMinLayerSpace :: Override LayeredDigraphLayout_ (LayeredDigraphVertex_ -> Boolean -> Effect Number)
  , assignLayers :: Override LayeredDigraphLayout_ (Effect Unit)
  )

type LayeredDigraphFields = LayoutSpecificFields LayeredDigraphLayout_ LayeredDigraphNetwork_ LayeredDigraphSpecificFields

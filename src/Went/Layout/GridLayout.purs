module Went.Layout.GridLayout where

import Prelude

import GoJS.GraphObject.Types (SomePart_)
import GoJS.Layout.Types (GridLayout_)
import Went.Geometry.Size (Size)
import Went.Layout (LayoutSpecificFields)
import Went.Layout.EnumValue.GridAlignment (Alignment)
import Went.Layout.EnumValue.GridArrangement (Arrangement)
import Went.Layout.EnumValue.GridSorting (Sorting)

type GridSpecificFields =
  ( alignment :: Alignment
  , arrangement :: Arrangement
  , cellSize :: Size
  , comparer :: SomePart_ -> SomePart_ -> Number
  , sorting :: Sorting
  , spacing :: Size
  , wrappingColumn :: Int
  , wrappingWidth :: Number
  )

-- Grid has no network, so it's impossible to override makeNetwork for it.
type GridFields = LayoutSpecificFields GridLayout_ Void GridSpecificFields

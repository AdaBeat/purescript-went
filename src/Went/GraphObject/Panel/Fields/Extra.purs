module Went.GraphObject.Panel.Fields.Extra where

import Went.Geometry.Margin (Margin)
import Went.Geometry.Point (Point)
import Went.Geometry.Size (Size)
import Went.GraphObject.EnumValue.ViewboxStretch (ViewboxStretch)
import Went.GraphObject.Panel.PanelType (Auto', Graduated', Grid', Horizontal', Link', PanelType, Position', Spot', Table', TableColumn', TableRow', Vertical', ViewBox')
import Went.RowColumnDefinition.EnumValue.Sizing (Sizing)

class ExtraFieldsPanel (panelType :: PanelType) (extraFields :: Row Type) | panelType -> extraFields

-- PanelTypes and their implied extra fields.
instance
  ExtraFieldsPanel
    Position'
    ()

instance
  ExtraFieldsPanel
    Horizontal'
    (isOpposite :: Boolean)

instance
  ExtraFieldsPanel
    Vertical'
    (isOpposite :: Boolean)

instance
  ExtraFieldsPanel
    Auto'
    ()

instance
  ExtraFieldsPanel
    Spot'
    ( alignmentFocusName :: String
    , isClipping :: Boolean )

instance
  ExtraFieldsPanel
    Table'
    ( columnSizing :: Sizing
    , defaultColumnSeparatorDashArray :: Array Number
    , defaultColumnSeparatorStroke :: String
    , defaultColumnSeparatorStrokeWidth :: Number
    , defaultRowSeparatorDashArray :: Array Number
    , defaultRowSeparatorStroke :: String
    , defaultRowSeparatorStrokeWidth :: Number
    , defaultSeparatorPadding :: Margin
    , leftIndex :: Int
    , rowSizing :: Sizing
    , topIndex :: Int
    )

instance
  ExtraFieldsPanel
    TableRow'
    ()

instance
  ExtraFieldsPanel
    TableColumn'
    ()

instance
  ExtraFieldsPanel
    ViewBox'
    (viewboxStretch :: ViewboxStretch)

instance
  ExtraFieldsPanel
    Grid'
    ( gridCellSize :: Size
    , gridOrigin :: Point
    )

instance
  ExtraFieldsPanel
    Link'
    ()

instance
  ExtraFieldsPanel
    Graduated'
    ( graduatedMax :: Number
    , graduatedMin :: Number
    , graduatedTickBase :: Number
    , graduatedTickUnit :: Number
    )

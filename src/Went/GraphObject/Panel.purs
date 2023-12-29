module Went.GraphObject.Panel where

import GoJS.GraphObject.Types

import Went.Geometry.Margin (Margin)
import Went.Geometry.Point (Point)
import Went.Geometry.Size (Size)
import Went.Geometry.Spot as Spot
import Went.GraphObject (GraphObjectSpecificFields)
import Went.GraphObject.EnumValue.SegmentOrientation (SegmentOrientation)
import Went.GraphObject.EnumValue.Stretch (Stretch)
import Went.GraphObject.EnumValue.ViewboxStretch (ViewboxStretch)
import Went.GraphObject.Shape.Arrowhead (Arrowhead)
import Went.GraphObject.Shape.Figure (Figure)
import Went.RowColumnDefinition.EnumValue.Sizing (Sizing)

type PanelSpecificFields :: Row Type -> Row Type
type PanelSpecificFields r =
  ( defaultAlignment :: Spot.Spot
  , defaultStretch :: Stretch
  , isEnabled :: Boolean
  , padding :: Margin
  -- TODO: itemTemplate-related fields are missing  
  | r
  )

type PanelFields (r :: Row Type) = GraphObjectSpecificFields Panel_ (PanelSpecificFields r)


newtype PanelTypeTag (panelType :: PanelType) panel = PanelTypeTag panel
unPanelTypeTag :: forall panelType panel. PanelTypeTag panelType panel -> panel
unPanelTypeTag (PanelTypeTag panel) = panel

instance IsGraphObject (PanelTypeTag p pa)
instance IsPanel (PanelTypeTag p pa)

newtype ButtonTypeTag (buttonType :: ButtonType) (panelType :: PanelType) button = ButtonTypeTag button
unButtonTypeTag :: forall buttonType panelType button. ButtonTypeTag buttonType panelType button -> button
unButtonTypeTag (ButtonTypeTag button) = button

instance IsGraphObject (ButtonTypeTag b p bpa)
instance IsPanel (ButtonTypeTag b p bpa)

-- | Helper class to convert "singleton" types to strings
class AsString :: forall k. k -> Constraint
class AsString p where
  asString :: String
-- | PanelTypes, expressed as types of kind PanelType
foreign import data PanelType :: Type
foreign import data Position' :: PanelType
foreign import data Horizontal' :: PanelType
foreign import data Vertical' :: PanelType
foreign import data Auto' :: PanelType
foreign import data Spot' :: PanelType
foreign import data Table' :: PanelType
foreign import data TableRow' :: PanelType
foreign import data TableColumn' :: PanelType
foreign import data ViewBox' :: PanelType
foreign import data Grid' :: PanelType
foreign import data Link' :: PanelType
foreign import data Graduated' :: PanelType
instance AsString Position' where asString = "Position"
instance AsString Horizontal' where asString = "Horizontal"
instance AsString Vertical' where asString = "Vertical"
instance AsString Auto' where asString = "Auto"
instance AsString Spot' where asString = "Spot"
instance AsString Table' where asString = "Table"
instance AsString TableRow' where asString = "TableRow"
instance AsString TableColumn' where asString = "TableColumn"
instance AsString ViewBox' where asString = "ViewBox"
instance AsString Grid' where asString = "Grid"
instance AsString Link' where asString = "Link"
instance AsString Graduated' where asString = "Graduated"




-- | ButtonTypes, expressed as types of kind ButtonType
foreign import data ButtonType :: Type
foreign import data Basic' :: ButtonType
foreign import data ContextMenu' :: ButtonType
foreign import data TreeExpander' :: ButtonType
foreign import data SubGraphExpander' :: ButtonType
foreign import data PanelExpander' :: ButtonType
foreign import data CheckBox' :: ButtonType
instance AsString Basic' where asString = "" -- There's no "name" for a basic button in GoJS
instance AsString ContextMenu' where asString = "ContextMenu"
instance AsString TreeExpander' where asString = "TreeExpander"
instance AsString SubGraphExpander' where asString = "SubGraphExpander"
instance AsString PanelExpander' where asString = "PanelExpander"



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
    , rowCount :: Int
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
    , graduatedRange :: Number
    , graduatedTickBase :: Number
    , graduatedTickUnit :: Number
    )

-- ButtonTypes and their implied extra fields
class ExtraFieldsButton (buttonType :: ButtonType) (extraFields :: Row Type) | buttonType -> extraFields

instance
  ExtraFieldsButton Basic'
    ()

instance
  ExtraFieldsButton ContextMenu'
    ()

instance
  ExtraFieldsButton TreeExpander'
    ( _treeExpandedFigure :: Figure
    , _treeCollapsedFigure :: Figure
    )

instance
  ExtraFieldsButton SubGraphExpander'
    ( _treeExpandedFigure :: Figure
    , _treeCollapsedFigure :: Figure
    )

instance
  ExtraFieldsButton PanelExpander'
    ()

instance
  ExtraFieldsButton CheckBox'
    ()

-- Extra fields for children of certain types that have parents certain panel types.
class ExtraFieldsChild (panelWithType :: Type) (child :: Type) (extraFields :: Row Type) | panelWithType child -> extraFields
instance
  ExtraFieldsChild (tag Table' panel ) anychild
    ( row :: Int
    , rowSpan :: Int
    , column :: Int
    , columnSpan :: Int
    )
else instance
  ExtraFieldsChild (tag Link' Link_) Shape_
    ( segmentFraction :: Number
    , segmentIndex :: Int
    , segmentOffset :: Point
    , segmentOrientation :: SegmentOrientation
    , isPanelMain :: Boolean
    , toArrow :: Arrowhead
    , fromArrow :: Arrowhead
    )
else instance
  ExtraFieldsChild (tag Link' Link_) anychild
    ( segmentFraction :: Number
    , segmentIndex :: Int
    , segmentOffset :: Point
    , segmentOrientation :: SegmentOrientation
    , isPanelMain :: Boolean
    )
else instance
  ExtraFieldsChild (tag Auto' panel) anychild
    ( isPanelMain :: Boolean
    )
else instance
  ExtraFieldsChild (tag Spot' panel) anychild
    ( isPanelMain :: Boolean
    )
else instance
  ExtraFieldsChild (tag TableRow' panel) anychild
    ( isPanelMain :: Boolean
    )
else instance
  ExtraFieldsChild (tag TableColumn' panel) anychild
    ( isPanelMain :: Boolean
    )
else instance
  ExtraFieldsChild (tag Graduated' panel) Shape_
    ( segmentOrientation :: SegmentOrientation
    , graduatedEnd :: Number
    , graduatedSkip :: Number -> Shape_ -> Boolean
    , graduatedStart :: Number
    , interval :: Number
    , isPanelMain :: Boolean
    )
else instance
  ExtraFieldsChild (tag Grid' panel) Shape_
    ( interval :: Number
    )
else instance
  ExtraFieldsChild (tag Graduated' panel) TextBlock_
    ( segmentOrientation :: SegmentOrientation
    , graduatedEnd :: Number
    , graduatedFunction :: Number -> TextBlock_ -> String
    , graduatedSkip :: Number -> TextBlock_ -> Boolean
    , graduatedStart :: Number
    , interval :: Number
    , isPanelMain :: Boolean
    )
else instance
  ExtraFieldsChild (tag Graduated' panel) anychild
    ( segmentOrientation :: SegmentOrientation
    , isPanelMain :: Boolean
    )
else instance
  ExtraFieldsChild k anychild ()

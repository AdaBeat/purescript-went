module Went.GraphObject.Panel.Fields.Specific where

import GoJS.GraphObject.Types (Panel_)
import Went.Geometry.Margin (Margin)
import Went.Geometry.Spot as Spot
import Went.GraphObject.EnumValue.Stretch (Stretch)
import Went.GraphObject.Fields.Specific (GraphObjectSpecificFields)


{- 
alignmentFocusName - Only for Spot
columnCount - Read-only
columnSizing - Only for Table
data - Read-only, in a way
defaultAlignment - 
defaultColumnSeparatorDashArray - Only for Table
defaultColumnSeparatorStroke - Only for Table
defaultColumnSeparatorStrokeWidth - Only for Table
defaultRowSeparatorDashArray - Only for Table
defaultRowSeparatorStroke - Only for Table
defaultRowSeparatorStrokeWidth - Only for Table
defaultSeparatorPadding - Only for Table
defaultStretch - 
elements - Read-only
graduatedMax - Only for Graduated
graduatedMin - Only for Graduated
graduatedRange - Read-only
graduatedTickBase - Only for Graduated
graduatedTickUnit - Only for Graduated
gridCellSize - Only for Grid
gridOrigin - Only for Grid
isClipping - Only for Spot
isEnabled - 
isOpposite - Only for Horizontal, Vertical
itemArray - 
itemCategoryProperty - 
itemIndex - 
itemTemplate - 
itemTemplateMap - 
leftIndex - Only for Table
padding - 
rowCount - Read-only
rowSizing - Only for Table
topIndex - Only for Table
type - Read-only
viewboxStretch - Only for ViewBox
-}


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

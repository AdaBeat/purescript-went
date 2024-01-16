module Went.GraphObject.Panel.Part.PlainFields where

import Prelude

import Effect (Effect)
import GoJS.Diagram.Types (Layer_)
import GoJS.Geometry.Types (Point_)
import GoJS.GraphObject.Types (Part_, Node_)
import Went.Geometry.Point (Point)
import Went.Geometry.Size (Size)
import Went.Geometry.Spot (Spot)
import Went.GraphObject.Fields.Specific (GraphObjectSpecificFields)
import Went.GraphObject.Panel.Fields.Specific (PanelSpecificFields)

{-
adornments - Read-only
category - 
containingGroup - Ignored, containingGroup should be defined in Model
containingGroupChanged - 
copyable - 
deletable - 
diagram - Read-only
dragComputation - 
groupable - 
highlightedChanged
isAnimated
isHighlighted
isInDocumentBounds
isLayoutPositioned
isSelected
isShadowed
isTopLevel - Read-only
key - Read-only
layer - Read-only
layerChanged
layerName
layoutConditions
location
locationObject - Read-only
locationObjectName
locationSpot
maxLocation
minLocation
movable
reshapable
resizable
resizeAdornmentTemplate - Monadic
resizeCellSize
resizeObject - Read-only
resizeObjectName
rotatable
rotateAdornmentTemplate
rotateObject
rotateObjectName
rotationSpot
selectable
selectionAdorned
selectionAdornmentTemplate - Monadic
selectionChanged
selectionObject - Read-only
selectionObjectName
shadowBlur
shadowColor
shadowOffset
text
textEditable
zOrder
-}

type PartSpecificFields (this :: Type) (a :: Row Type) =
  ( category :: String
  , containingGroupChanged :: this -> Node_ -> Node_ -> Effect Unit
  , copyable :: Boolean
  , deletable :: Boolean
  , dragComputation :: this -> Point_ -> Point_ -> Point_
  , groupable :: Boolean
  , highlightChanged :: this -> Effect Unit
  , isAnimated :: Boolean
  , isHighlighted :: Boolean
  , isInDocumentBounds :: Boolean
  , isLayoutPositioned :: Boolean
  , isSelected :: Boolean
  , isShadowed :: Boolean
  , layerChanged :: this -> Layer_ -> Layer_ -> Effect Unit
  , layerName :: String
  , layerConditions :: Boolean
  , location :: Point
  , locationObjectName :: String
  , locationSpot :: Spot
  , maxLocation :: Point
  , minLocation :: Point
  , movable :: Boolean
  , reshapable :: Boolean
  , resizable :: Boolean
  , resizeCellSize :: Size
  , resizeObjectName :: String
  , rotatable :: Boolean
  , rotateObjectName :: String
  , rotationSpot :: Spot
  , selectable :: Boolean
  , selectionAdorned :: Boolean
  , selectionChanged :: this -> Effect Unit
  , selectionObjectName :: String
  , shadowBlur :: Number
  , shadowColor :: String
  , shadowOffset :: Point
  , text :: String
  , textEditable :: Boolean
  , zOrder :: Int
  | a
  )

type PartFields (this :: Type) (extraFields :: Row Type) = GraphObjectSpecificFields Part_
  ( PanelSpecificFields
      ( PartSpecificFields this extraFields
      )
  )

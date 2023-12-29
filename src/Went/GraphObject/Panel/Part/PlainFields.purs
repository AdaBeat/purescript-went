module Went.GraphObject.Panel.Part.PlainFields where

import Prelude

import Effect (Effect)
import GoJS.Diagram.Types (Layer_)
import GoJS.Geometry.Types (Point_)
import GoJS.GraphObject.Types (Part_, SomeNode_, SomePart_)
import Went.Geometry.Point (Point)
import Went.Geometry.Size (Size)
import Went.Geometry.Spot (Spot)
import Went.GraphObject (GraphObjectSpecificFields)
import Went.GraphObject.Panel (PanelSpecificFields)


type PartSpecificFields (a :: Row Type) =
  ( category :: String
  , containingGroupChanged :: SomePart_ -> SomeNode_ -> SomeNode_ -> Effect Unit
  , copyable :: Boolean
  , deletable :: Boolean
  , dragComputation :: SomePart_ -> Point_ -> Point_ -> Point_
  , groupable :: Boolean
  , highlightChanged :: SomePart_ -> Effect Unit
  , isAnimated :: Boolean
  , isHighlighted :: Boolean
  , isInDocumentBounds :: Boolean
  , isLayoutPositioned :: Boolean
  , isSelected :: Boolean
  , isShadowed :: Boolean
  , layerChanged :: SomePart_ -> Layer_ -> Layer_ -> Effect Unit
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
  , selectionChanged :: SomePart_ -> Effect Unit
  , selectionObjectName :: String
  , shadowBlur :: Number
  , shadowColor :: String
  , shadowOffset :: Point
  , text :: String
  , textEditable :: Boolean
  , zOrder :: Int
  | a
  )

type PartFields (extraFields :: Row Type) = GraphObjectSpecificFields Part_
  ( PanelSpecificFields
      ( PartSpecificFields extraFields
      )
  )

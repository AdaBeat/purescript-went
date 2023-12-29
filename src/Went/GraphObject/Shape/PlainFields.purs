module Went.GraphObject.Shape.PlainFields where

import GoJS.GraphObject.Types (Shape_)
import Went.Geometry.Geometry (Geometry)
import Went.Geometry.Spot (Spot)
import Went.GraphObject (GraphObjectSpecificFields)
import Went.GraphObject.EnumValue.GeometryStretch (GeometryStretch)
import Went.GraphObject.Shape.Figure (Figure)
import Went.GraphObject.Shape.StrokeCap (StrokeCap)
import Went.GraphObject.Shape.StrokeJoin (StrokeJoin)

type ShapeSpecificFields =
  ( figure :: Figure
  , fill :: String
  , geometry :: Geometry
  , geometryStretch :: GeometryStretch
  , geometryString :: String
  , interval :: Number
  , isGeometryPositioned :: Boolean
  , parameter1 :: Number
  , parameter2 :: Number
  , spot1 :: Spot
  , spot2 :: Spot
  , stroke :: String
  , strokeCap :: StrokeCap
  , strokeDashArray :: Array Number
  , strokeDashOffset :: Number
  , strokeJoin :: StrokeJoin
  , strokeMiterLimit :: Number
  , strokeWidth :: Number
  -- Fields that only make sense when a shape is in a Link.
  -- , fromArrow :: String
  -- , toArrow :: String
  -- Fields that only make sense when a shape is in a Graduated Panel
  -- , graduatedEnd :: Number
  -- , graduatedSkip :: Number -> Shape_ -> Boolean
  -- , graduatedStart :: Number
  )

-- pathPattern

type ShapeFields = GraphObjectSpecificFields Shape_ ShapeSpecificFields

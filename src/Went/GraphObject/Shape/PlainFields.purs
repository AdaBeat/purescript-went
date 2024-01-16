module Went.GraphObject.Shape.PlainFields where

import GoJS.GraphObject.Types (Shape_)
import Went.Geometry.Geometry (Geometry)
import Went.Geometry.Spot (Spot)
import Went.GraphObject.Fields.Specific (GraphObjectSpecificFields)
import Went.GraphObject.EnumValue.GeometryStretch (GeometryStretch)
import Went.GraphObject.Shape.Figure (Figure)
import Went.GraphObject.Shape.StrokeCap (StrokeCap)
import Went.GraphObject.Shape.StrokeJoin (StrokeJoin)

{-
figure
fill
fromArrow -- Only when in a Link
geometry
geometryStretch
geometryString
graduatedEnd -- Only when in a Graduated Panel
graduatedSkip -- Only when in a Graduated Panel
graduatedStart -- Only when in a Graduated Panel
interval -- Only when in a Grid or Graduated Panel
isGeometryPositioned
naturalBounds -- Read-only
parameter1
parameter2
pathPattern -- Monadic
spot1
spot2
stroke
strokeCap
strokeDashArray
strokeDashOffset
strokeJoin
strokeMiterLimit
strokeWidth
toArrow -- Only when in a Link

-}

type ShapeSpecificFields =
  ( figure :: Figure
  , fill :: String
  , geometry :: Geometry
  , geometryStretch :: GeometryStretch
  , geometryString :: String
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
  )

type ShapeFields = GraphObjectSpecificFields Shape_ ShapeSpecificFields

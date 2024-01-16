module Went.GraphObject.Panel.Part.Link where

import Prelude

import Effect (Effect)
import GoJS.GraphObject.Types (Link_, GraphObject_)
import Went.Geometry.Point (Point)
import Went.GraphObject.Fields.Specific (GraphObjectSpecificFields)
import Went.GraphObject.EnumValue.Adjusting (Adjusting)
import Went.GraphObject.EnumValue.Curve (Curve)
import Went.GraphObject.EnumValue.Routing (Routing)
import Went.GraphObject.EnumValue.SegmentOrientation (SegmentOrientation)
import Went.GraphObject.Panel.Fields.Specific (PanelSpecificFields)
import Went.GraphObject.Panel.Part (PartSpecificFields)

{-
adjusting
corner
curve
curviness
fromEndSegmentLength
fromNode -- Read-only, in a way
fromPort -- Read-only
fromPortChanged
fromPortId -- Can be bound
fromShortLength -- Override
fromSpot -- Override
geometry -- Read-only
isLabeledLink -- Read-only
isOrthogonal -- Read-only
isTreeLink
key -- Read-only
labelNodes -- Read-only
midAngle -- Read-only
midPoint -- Read-only
path -- Read-only
points -- Can be bound
pointsCount -- Read-only
relinkableFrom
relinkableTo
resegmentable
routeBounds -- Read-only
routing
smoothness
toEndSegmentLength -- Override
toNode -- Read-only, in a way
toPort -- Read-only
toPortChanged
toPortId -- Can be bound
toShortLength -- Override
toSpot -- Override
-}

type LinkSpecificFields =
  ( adjusting :: Adjusting
  , corner :: Number
  , curve :: Curve
  , curviness :: Number
  , fromPortChanged :: Link_ -> GraphObject_ -> GraphObject_ -> Effect Unit
  , fromPortId :: String
  , fromShortLength :: Number
  , isTreeLink :: Boolean
  , points :: Array Point
  , relinkableFrom :: Boolean
  , relinkableTo :: Boolean
  , resegmentable :: Boolean
  , routing :: Routing
  , segmentFraction :: Number
  , segmentIndex :: Int
  , segmentOffset :: Point
  , segmentOrientation :: SegmentOrientation
  , isPanelMain :: Boolean
  , smoothness :: Number
  , toPortChanged :: Link_ -> GraphObject_ -> GraphObject_ -> Effect Unit
  , toPortId :: String
  )

type LinkFields = GraphObjectSpecificFields Link_
  ( PanelSpecificFields
      ( PartSpecificFields Link_ LinkSpecificFields
      )
  )

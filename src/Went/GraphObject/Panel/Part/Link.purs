module Went.GraphObject.Panel.Part.Link where

import Prelude

import Effect (Effect)
import GoJS.GraphObject.Types (Link_, SomeGraphObject_)
import Went.Geometry.Point (Point)
import Went.GraphObject (GraphObjectSpecificFields)
import Went.GraphObject.EnumValue.Adjusting (Adjusting)
import Went.GraphObject.EnumValue.Curve (Curve)
import Went.GraphObject.EnumValue.Routing (Routing)
import Went.GraphObject.EnumValue.SegmentOrientation (SegmentOrientation)
import Went.GraphObject.Panel (PanelSpecificFields)
import Went.GraphObject.Panel.Part (PartSpecificFields)

type LinkSpecificFields =
  ( adjusting :: Adjusting
  , corner :: Number
  , curve :: Curve
  , curviness :: Number
  , fromEndSegmentLength :: Number
  , fromPortChanged :: Link_ -> SomeGraphObject_ -> SomeGraphObject_ -> Effect Unit
  , fromPortId :: String
  , fromShortLength :: Number
  , isTreeLink :: Boolean
  , points :: Array Point
  , relinkableFrom :: Boolean
  , relinkableTo :: Boolean
  , resegmentable :: Boolean
  , routing :: Routing
  , smoothness :: Number
  , segmentFraction :: Number
  , segmentIndex :: Int
  , segmentOffset :: Point
  , segmentOrientation :: SegmentOrientation
  , toEndSegmentLength :: Number
  , toPortChanged :: Link_ -> SomeGraphObject_ -> SomeGraphObject_ -> Effect Unit
  , toPortId :: String
  , toShortLength :: Number
  )

-- fromNode, toNode

type LinkFields = GraphObjectSpecificFields Link_
  ( PanelSpecificFields
      ( PartSpecificFields LinkSpecificFields
      )
  )

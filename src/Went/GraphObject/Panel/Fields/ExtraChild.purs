module Went.GraphObject.Panel.Fields.ExtraChild where


import GoJS.GraphObject.Types (Link_, Shape_, TextBlock_)
import Went.Geometry.Point (Point)
import Went.GraphObject.EnumValue.SegmentOrientation (SegmentOrientation)
import Went.GraphObject.Panel.PanelType (Auto', Graduated', Grid', Link', Spot', Table', TableColumn', TableRow')
import Went.GraphObject.Shape.Arrowhead (Arrowhead)

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
  ExtraFieldsChild (tag Grid' panel) TextBlock_
    ( interval :: Number
    )
else instance
  ExtraFieldsChild (tag Graduated' panel) anychild
    ( segmentOrientation :: SegmentOrientation
    , isPanelMain :: Boolean
    )
-- else instance (IsPart panel) =>
--   ExtraFieldsChild (tag panelType panel) anychild
--     ( shadowVisible :: Boolean
--     , isPanelMain :: Boolean
--     )
else instance
  ExtraFieldsChild k anychild ()
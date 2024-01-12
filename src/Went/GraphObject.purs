module Went.GraphObject where

import Prelude

import Effect (Effect)
import GoJS.Diagram.Types (Diagram_, InputEvent_)
import GoJS.GraphObject.Types (GraphObject_)
import Went.Geometry.Margin (Margin)
import Went.Geometry.Point (Point)
import Went.Geometry.Size (Size)
import Went.Geometry.Spot (Spot)
import Went.GraphObject.EnumValue.Stretch (Stretch)

type GraphObjectSpecificFields (this :: Type) (r :: Row Type) =
  ( actionCancel :: InputEvent_ Diagram_ -> this -> Effect Unit
  , actionDown :: InputEvent_ Diagram_ -> this -> Effect Unit
  , actionMove :: InputEvent_ Diagram_ -> this -> Effect Unit
  , actionUp :: InputEvent_ Diagram_ -> this -> Effect Unit
  , alignment :: Spot
  , alignmentFocus :: Spot
  , angle :: Number
  , background :: String
  , click :: InputEvent_ Diagram_ -> this -> Effect Unit
  , contextClick :: InputEvent_ Diagram_ -> this -> Effect Unit
  , cursor :: String
  , desiredSize :: Size
  , doubleClick :: InputEvent_ Diagram_ -> this -> Effect Unit
  , enabledChange :: this -> Boolean -> Effect Unit
  , fromEndSegmentLength :: Number
  , fromLinkable :: Boolean
  , fromLinkableDuplicates :: Boolean
  , fromLinkableSelfNode :: Boolean
  , fromMaxLinks :: Int
  , fromShortLength :: Number
  , fromSpot :: Spot
  , height :: Number
  , isActionable :: Boolean
  , margin :: Margin
  , maxSize :: Size
  , minSize :: Size
  , mouseDragEnter :: InputEvent_ Diagram_ -> this -> GraphObject_ -> Effect Unit
  , mouseDragLeave :: InputEvent_ Diagram_ -> this -> GraphObject_ -> Effect Unit
  , mouseDrop :: InputEvent_ Diagram_ -> this -> Effect Unit
  , mouseEnter :: InputEvent_ Diagram_ -> this -> GraphObject_ -> Effect Unit
  , mouseHold :: InputEvent_ Diagram_ -> this -> Effect Unit
  , mouseHover :: InputEvent_ Diagram_ -> this -> Effect Unit
  , mouseLeave :: InputEvent_ Diagram_ -> this -> GraphObject_ -> Effect Unit
  , mouseOver :: InputEvent_ Diagram_ -> this -> Effect Unit
  , name :: String
  , opacity :: Number
  , pickable :: Boolean
  , portId :: String
  , position :: Point
  , scale :: Number
  , stretch :: Stretch
  , toEndSegmentLength :: Number
  , toLinkable :: Boolean
  , toLinkableDuplicates :: Boolean
  , toLinkableSelfNode :: Boolean
  , toMaxLinks :: Int
  , toShortLength :: Number
  , toSpot :: Spot
  , visible :: Boolean
  , width :: Number
  -- Fields that are constructed out of "Makeable" data are implemented as functions in the
  -- relevant monad.
  -- contextMenu :: MakeSetAdornment
  -- toolTip :: MakeSetAdornment
  -- Fields that only make sense when this graph object is in a Table panel's tree.
  -- row :: Int
  -- column :: Int
  -- rowSpan :: Int
  -- columnSpan :: Int
  -- Fields that only make sense when this graph object is in a Link's tree, or is a link.
  -- , segmentFraction :: Number
  -- , segmentIndex :: Int
  -- , segmentOffset :: Point
  -- , segmentOrientation :: SegmentOrientation
  -- Only makes sense when child of some panels.
  -- , isPanelMain :: Boolean
  -- , shadowVisible :: Boolean

  | r
  )

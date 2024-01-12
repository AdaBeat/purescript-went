module Went.Layout where

import Prelude

import Effect (Effect)
import GoJS.Diagram.Types (Diagram_)
import GoJS.Geometry.Types (Point_, Rect_)
import GoJS.GraphObject.Types (Part_)
import Went.FFI.Override (Override)
import Went.Geometry.Point (Point)

type LayoutSpecificFields (this :: Type) (network :: Type) (r :: Row Type) =
  ( arrangementOrigin :: Point
  , boundsComputation :: Part_ -> this -> Rect_ -> Rect_
  , isInitial :: Boolean
  , isOngoing :: Boolean
  , isRealtime :: Boolean
  , isRouting :: Boolean
  , isValidLayout :: Boolean
  , isViewportSized :: Boolean
  -- Methods
  , commitLayout :: Override this (Effect Unit)
  , commitNodes :: Override this (Effect Unit)
  , createNetwork :: Override this (Effect Unit)
  , doLayout :: Override this (Diagram_ -> Effect Unit)
  , getLayoutBounds :: Override this (Part_ -> Effect Rect_)
  , initialOrigin :: Override this (Point_ -> Effect Point_)
  , invalidateLayout :: Override this (Effect Unit)
  , makeNetwork :: Override this (Diagram_ -> Effect network)
  , updateParts :: Override this (Effect Unit)
  | r
  )

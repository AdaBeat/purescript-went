module Went.Layout where

import Prelude

import Effect (Effect)
import GoJS.Diagram.Types (Diagram_)
import GoJS.Geometry.Types (Rect_)
import GoJS.GraphObject.Types (SomePart_)
import Went.FFI.Override (Override)
import Went.Geometry.Point (Point)

type LayoutSpecificFields (this :: Type) (network :: Type) (r :: Row Type) =
  ( arrangementOrigin :: Point
  , boundsComputation :: SomePart_ -> this -> Rect_ -> Rect_
  , isInitial :: Boolean
  , isOngoing :: Boolean
  , isRealtime :: Boolean
  , isRouting :: Boolean
  , isValidLayout :: Boolean
  , isViewportSized :: Boolean
  -- Methods
  , commitNodes :: Override this (Effect Unit)
  , commitLayout :: Override this (Effect Unit)
  , createNetwork :: Override this (Effect Unit)
  , doLayout :: Override this (Diagram_ -> Effect Unit)
  , makeNetwork :: Override this (Diagram_ -> Effect network)
  , updateParts :: Override this (Effect Unit)
  -- TODO: Missing: invalidateLayout, initialOrigin, getLayoutBounds
  | r
  )

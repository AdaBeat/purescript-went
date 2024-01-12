module Went.Tool where

import Prelude

import Effect (Effect)
import GoJS.Diagram.Types (Diagram_, InputEvent_)
import GoJS.Geometry.Types (Point_)
import GoJS.GraphObject.Types (GraphObject_, Part_)
import Went.FFI.Override (Override)

type ToolSpecificFields (this :: Type) (r :: Row Type) =
  ( isActive :: Boolean -- TODO: Doesn't make much sense to set this at a tool config moment
  , isEnabled :: Boolean
  , name :: String
  , transactionResult :: String -- Ditto
  -- Overridable methods
  , canStart :: Override this (Effect Boolean)
  , canStartMultiTouch :: Override this (Effect Boolean)
  , cancelWaitAfter :: Override this (Effect Unit)
  , doActivate :: Override this (Effect Unit)
  , doCancel :: Override this (Effect Unit)
  , doDeactivate :: Override this (Effect Unit)
  , doKeyDown :: Override this (Effect Unit)
  , doKeyUp :: Override this (Effect Unit)
  , doMouseDown :: Override this (Effect Unit)
  , doMouseMove :: Override this (Effect Unit)
  , doMouseUp :: Override this (Effect Unit)
  , doMouseWheel :: Override this (Effect Unit)
  , doStart :: Override this (Effect Unit)
  , doStop :: Override this (Effect Unit)
  , doWaitAfter :: Override this (InputEvent_ Diagram_ -> Effect Unit)
  , findToolHandleAt :: Override this (Point_ -> String -> Effect GraphObject_)
  , isBeyondDragSize :: Override this (Point_ -> Point_ -> Effect Boolean)
  -- TODO: Figure out type of standardMouseClick
  -- , standardMouseClick
  , standardMouseOver :: Override this (Effect Unit)
  , standardMouseSelect :: Override this (Effect Unit)
  , standardMouseWheel :: Override this (Effect Unit)
  , standardPinchZoomMove :: Override this (Effect Unit)
  , standardPinchZoomStart :: Override this (Effect Unit)
  , standardWaitAfter :: Override this (Effect Unit)
  , startTransaction :: Override this (String -> Effect Boolean)
  , stopTool :: Override this (Effect Unit)
  , stopTransaction :: Override this (Effect Boolean)
  , updateAdornments :: Override this (Part_ -> Effect Unit)
  | r
  )

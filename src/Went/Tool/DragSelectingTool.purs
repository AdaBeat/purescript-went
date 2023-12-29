module Went.Tool.DragSelectingTool where

import Prelude

import Effect (Effect)
import GoJS.Geometry.Types (Rect_)
import GoJS.Tool.Types (DragSelectingTool_)
import Went.FFI.Override (Override)
import Went.Tool (ToolSpecificFields)

type DragSelectingToolSpecificFields =
  ( 
    -- box :: Part_ : TODO: This should be a function running inside MakeTool monad
    delay :: Number
  , isPartialInclusion :: Boolean
  , isDoubleClick :: Boolean
  , isGridSnapEnabled :: Boolean
  -- Extra overridable method
  , computeBoxBounds :: Override DragSelectingTool_ (Effect Rect_)
  , selectInRect :: Override DragSelectingTool_ (Rect_ -> Effect Unit)
  )

type DragSelectingToolFields = ToolSpecificFields DragSelectingTool_ DragSelectingToolSpecificFields

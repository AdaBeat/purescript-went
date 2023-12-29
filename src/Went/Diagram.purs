module Went.Diagram where

import Went.Diagram.EnumValue.AutoScale (AutoScale)
import Went.Diagram.EnumValue.GestureBehavior (GestureBehavior)
import Went.Diagram.EnumValue.InitialAnimationStyle (InitialAnimationStyle)
import Went.Diagram.EnumValue.MouseWheelBehavior (MouseWheelBehavior)
import Went.Diagram.EnumValue.ValidCycle (ValidCycle)
import Went.Geometry.Point (Point)
import Went.Geometry.Size (Size)
import Went.Geometry.Spot (Spot)

type DiagramAttachOptions =
  ( "grid.visible" :: Boolean
  , "grid.gridCellSize" :: Size
  , "grid.gridOrigin" :: Point
  , allowZoom :: Boolean
  , "animationManager.isEnabled" :: Boolean
  , "commandHandler.copiesTree" :: Boolean
  , "commandHandler.deletesTree" :: Boolean
  , "toolManager.mouseWheelBehavior" :: MouseWheelBehavior
  , "toolManager.gestureBehavior" :: GestureBehavior
  --  , "toolManager.currentToolTip" :: Adornment -- | HTMLInfo | null
  , "animationManager.initialAnimationStyle" :: InitialAnimationStyle
  , "undoManager.isEnabled" :: Boolean
  , "draggingTool.dragsTree" :: Boolean
  , "draggingTool.horizontalGuidelineColor" :: String -- Color
  , "draggingTool.verticalGuidelineColor" :: String -- Color
  , "draggingTool.centerGuidelineColor" :: String -- Color
  , "draggingTool.guidelineWidth" :: Int
  , validCycle :: ValidCycle
  , maxSelectionCount :: Int
  , allowCopy :: Boolean
  , allowDelete :: Boolean
  , initialAutoScale :: AutoScale
  , initialContentAlignment :: Spot
  )

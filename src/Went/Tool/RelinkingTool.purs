module Went.Tool.RelinkingTool where


import GoJS.Tool.Types (RelinkingTool_)
import Went.Tool (ToolSpecificFields)
import Went.Tool.LinkingBaseTool (LinkingBaseToolSpecificFields)

type RelinkingToolSpecificFields :: forall k. Row k
type RelinkingToolSpecificFields =
  ()

type RelinkingToolFields :: forall k. k -> Row Type
type RelinkingToolFields nodeData = ToolSpecificFields RelinkingTool_ (LinkingBaseToolSpecificFields RelinkingToolSpecificFields)

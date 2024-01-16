module Went.GraphObject.Panel.Button.Fields.Extra where

import Went.GraphObject.Panel.Button.ButtonType (Basic', ButtonType, CheckBox', ContextMenu', PanelExpander', SubGraphExpander', TreeExpander')
import Went.GraphObject.Shape.Figure (Figure)

-- ButtonTypes and their implied extra fields
class ExtraFieldsButton (buttonType :: ButtonType) (extraFields :: Row Type) | buttonType -> extraFields

instance
  ExtraFieldsButton Basic'
    ()

instance
  ExtraFieldsButton ContextMenu'
    ()

instance
  ExtraFieldsButton TreeExpander'
    ( _treeExpandedFigure :: Figure
    , _treeCollapsedFigure :: Figure
    )

instance
  ExtraFieldsButton SubGraphExpander'
    ( _treeExpandedFigure :: Figure
    , _treeCollapsedFigure :: Figure
    )

instance
  ExtraFieldsButton PanelExpander'
    ()

instance
  ExtraFieldsButton CheckBox'
    ()
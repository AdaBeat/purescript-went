module Went.GraphObject.Fields where

import GoJS.GraphObject.Types (class IsGraphObject, Adornment_, Button_, Group_, Link_, Node_, Panel_, Part_, Picture_, Placeholder_, Shape_, TextBlock_)
import Prim.Row (class Union)
import Type.Data.List (type (:>), List', Nil')
import Went.GraphObject.Panel (class ExtraFieldsButton, class ExtraFieldsChild, class ExtraFieldsPanel, ButtonTypeTag, Link', PanelFields, PanelTypeTag)
import Went.GraphObject.Panel.Part (PartFields)
import Went.GraphObject.Panel.Part.Adornment (AdornmentFields)
import Went.GraphObject.Panel.Part.Link (LinkFields)
import Went.GraphObject.Panel.Part.Node (NodeFields)
import Went.GraphObject.Panel.Part.Node.Group (GroupFields)
import Went.GraphObject.Picture (PictureFields)
import Went.GraphObject.Placeholder (PlaceholderFields)
import Went.GraphObject.Shape.PlainFields (ShapeFields)
import Went.GraphObject.TextBlock.PlainFields (TextBlockFields)

class IsGraphObject graphObjectType <= GraphObjectFields (graphObjectType :: Type) (extraFields :: Row Type) | graphObjectType -> extraFields


instance
  ( ExtraFieldsPanel panelType extra
  ) =>
  GraphObjectFields (PanelTypeTag panelType Panel_) (PanelFields extra)

instance
  ( ExtraFieldsPanel panelType extra
  ) =>
  GraphObjectFields (PanelTypeTag panelType Part_) (PartFields extra)

instance
  ( ExtraFieldsPanel panelType extra
  ) =>
  GraphObjectFields (PanelTypeTag panelType Node_) (NodeFields extra)

instance
  ( ExtraFieldsPanel panelType extra
  ) =>
  GraphObjectFields (PanelTypeTag panelType Adornment_) (AdornmentFields extra)

instance
  ( ExtraFieldsPanel panelType extra
  ) =>
  GraphObjectFields (PanelTypeTag panelType Group_) (GroupFields extra)

instance
  ( ExtraFieldsButton buttonType extraButton
  , ExtraFieldsPanel panelType extraPanel
  , Union extraButton extraPanel extra
  ) =>
  GraphObjectFields (ButtonTypeTag buttonType panelType Button_) (PanelFields extra)

instance GraphObjectFields Link_ LinkFields
instance GraphObjectFields (PanelTypeTag Link' Link_) LinkFields
instance GraphObjectFields Shape_ ShapeFields
instance GraphObjectFields Placeholder_ PlaceholderFields
instance GraphObjectFields TextBlock_ TextBlockFields
instance GraphObjectFields Picture_ PictureFields

class GraphObjectChildFields (curGO :: Type) (hierarchy :: List' Type) (extraFields :: Row Type) | hierarchy -> extraFields

instance
  ( ExtraFieldsChild latestPanel curGO childSettable
  ) =>
  GraphObjectChildFields curGO (latestPanel :> keys) childSettable
else instance GraphObjectChildFields k Nil' ()

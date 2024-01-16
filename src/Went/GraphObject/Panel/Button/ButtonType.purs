module Went.GraphObject.Panel.Button.ButtonType where

import GoJS.GraphObject.Types (class IsGraphObject, class IsPanel)
import Unsafe.Coerce (unsafeCoerce)
import Went.GraphObject.Panel.AsString (class AsString)
import Went.GraphObject.Panel.PanelType (PanelType)


-- | ButtonTypes, expressed as types of kind ButtonType
foreign import data ButtonType :: Type
foreign import data Basic' :: ButtonType
foreign import data ContextMenu' :: ButtonType
foreign import data TreeExpander' :: ButtonType
foreign import data SubGraphExpander' :: ButtonType
foreign import data PanelExpander' :: ButtonType
foreign import data CheckBox' :: ButtonType
instance AsString Basic' where asString = "" -- There's no "name" for a basic button in GoJS
instance AsString ContextMenu' where asString = "ContextMenu"
instance AsString TreeExpander' where asString = "TreeExpander"
instance AsString SubGraphExpander' where asString = "SubGraphExpander"
instance AsString PanelExpander' where asString = "PanelExpander"

newtype ButtonTypeTag (buttonType :: ButtonType) (panelType :: PanelType) button = ButtonTypeTag button
unButtonTypeTag :: forall buttonType panelType button. ButtonTypeTag buttonType panelType button -> button
unButtonTypeTag (ButtonTypeTag button) = button

instance IsGraphObject (ButtonTypeTag b p bpa)
instance IsPanel (ButtonTypeTag b p bpa) where
  fromPanel = unsafeCoerce
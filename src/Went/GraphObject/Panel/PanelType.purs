module Went.GraphObject.Panel.PanelType where

import GoJS.GraphObject.Types (class IsGraphObject, class IsPanel)
import Unsafe.Coerce (unsafeCoerce)
import Went.GraphObject.Panel.AsString (class AsString)

-- | PanelTypes, expressed as types of kind PanelType

foreign import data PanelType :: Type
foreign import data Position' :: PanelType
foreign import data Horizontal' :: PanelType
foreign import data Vertical' :: PanelType
foreign import data Auto' :: PanelType
foreign import data Spot' :: PanelType
foreign import data Table' :: PanelType
foreign import data TableRow' :: PanelType
foreign import data TableColumn' :: PanelType
foreign import data ViewBox' :: PanelType
foreign import data Grid' :: PanelType
foreign import data Link' :: PanelType
foreign import data Graduated' :: PanelType
instance AsString Position' where asString = "Position"
instance AsString Horizontal' where asString = "Horizontal"
instance AsString Vertical' where asString = "Vertical"
instance AsString Auto' where asString = "Auto"
instance AsString Spot' where asString = "Spot"
instance AsString Table' where asString = "Table"
instance AsString TableRow' where asString = "TableRow"
instance AsString TableColumn' where asString = "TableColumn"
instance AsString ViewBox' where asString = "ViewBox"
instance AsString Grid' where asString = "Grid"
instance AsString Link' where asString = "Link"
instance AsString Graduated' where asString = "Graduated"

newtype PanelTypeTag (panelType :: PanelType) panel = PanelTypeTag panel
unPanelTypeTag :: forall panelType panel. PanelTypeTag panelType panel -> panel
unPanelTypeTag (PanelTypeTag panel) = panel

instance IsGraphObject (PanelTypeTag p pa)
instance IsPanel (PanelTypeTag p pa) where
  fromPanel = unsafeCoerce
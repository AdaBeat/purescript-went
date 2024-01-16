module Went.RowColumnDefinition.Make where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Effect (Effect)
import Effect.Class (liftEffect)
import GoJS.GraphObject.Types (class IsPanel)
import GoJS.GraphObject.Panel.Methods (addRowColumnDefinition_)
import GoJS.RowColumnDefinition.Constructors (newRowColumnDefinition)
import GoJS.RowColumnDefinition.Methods (bind_)
import GoJS.RowColumnDefinition.Types (RowColumnDefinition_)
import Prim.Row (class Union)
import Went.GraphObject.Make (MakeGraphObject(..))
import Went.GraphObject.Panel.PanelType (PanelTypeTag, Table')
import Went.Model.Binding (class Bindable, bindingImp, bindingOfObjectImp)
import Went.RowColumnDefinition (RowColumnDefinitionFields)
import Went.Settable (class Settable, setImp)

newtype MakeRowColumnDefinition (bindableData :: Row Type) (a :: Type) = MakeRowColumnDefinition (ReaderT RowColumnDefinition_ Effect a)

derive newtype instance Functor (MakeRowColumnDefinition bindableData)
derive newtype instance Apply (MakeRowColumnDefinition bindableData)
derive newtype instance Applicative (MakeRowColumnDefinition bindableData)
derive newtype instance Monad (MakeRowColumnDefinition bindableData)
derive newtype instance Bind (MakeRowColumnDefinition bindableData)

rowColumnDefinition
  :: forall nodeData parentHierarchy p b inp rest
   . Union inp rest (column :: Int, row :: Int)
  => IsPanel p
  => Record inp
  -> MakeRowColumnDefinition nodeData b
  -> MakeGraphObject nodeData (PanelTypeTag Table' p) parentHierarchy Unit
rowColumnDefinition rowColInit (MakeRowColumnDefinition howToMakeRowColumnDefinition) = MakeGraphObject $ do
  pan <- ask
  rowCol <- liftEffect $ newRowColumnDefinition rowColInit >>= runReaderT (howToMakeRowColumnDefinition *> ask)
  _ <- liftEffect $ addRowColumnDefinition_ rowCol pan
  pure unit

instance Settable (MakeRowColumnDefinition bindableData) RowColumnDefinitionFields where
  set f = MakeRowColumnDefinition $ setImp f

instance Bindable (MakeRowColumnDefinition bindableData) bindableData RowColumnDefinitionFields where
  binding' ptgt prsc go back = MakeRowColumnDefinition $ bindingImp bind_  ptgt prsc go back
  bindingOfObject' ptgt src go back = MakeRowColumnDefinition $ bindingOfObjectImp bind_ ptgt src go back

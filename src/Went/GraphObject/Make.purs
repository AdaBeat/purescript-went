module Went.GraphObject.Make where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect (Effect)
import Effect.Class (liftEffect)
import GoJS.Diagram.AnimationTrigger.Constructors (newAnimationTrigger'')
import GoJS.GraphObject.Constructors (newButton, newContextMenu, newGroup, newLink, newNode, newPanel, newPart, newPicture, newPlaceholder, newShape, newTextBlock, newToolTip)
import GoJS.GraphObject.Methods (bind_, trigger_)
import GoJS.GraphObject.Panel.Methods (add_)
import GoJS.GraphObject.Types (class IsGraphObject, class IsPanel, Adornment_, Button_, Group_, Link_, Node_, Panel_, Part_, Picture_, Placeholder_, Shape_, TextBlock_)
import GoJS.Unsafe.Set (setUnsafe)
import Prim.Row (class Cons, class Union)
import Record (insert)
import Type.Data.List (type (:>), List', Nil')
import Type.Prelude (Proxy(..))
import Went.Diagram.Animation.AnimationTrigger (class AnimationTriggerable)
import Went.FFI.Class (ffi)
import Went.GraphObject.Fields.All (class GraphObjectChildFields, class GraphObjectAllFields)
import Went.GraphObject.Panel.AsString
import Went.GraphObject.Panel.PanelType
import Went.GraphObject.Panel.Button.ButtonType
import Went.GraphObject.Shape.Figure (Figure)
import Went.Layout.Make (class LayoutM, layoutImp)
import Went.Model.Binding (class Bindable, bindingImp, bindingOfObjectImp)
import Went.Settable (class Settable, setImp)
import Went.Template.Makers (Made(..))

-- TODO: How to make the head of hierarchy always be the reader's type?
newtype MakeGraphObject
  (bindable :: Row Type)
  (grObj :: Type)
  (hierarchy :: List' Type)
  (a :: Type) =
  MakeGraphObject (ReaderT grObj Effect a)

derive newtype instance Functor (MakeGraphObject bindable grObj hierarchy)
derive newtype instance Apply (MakeGraphObject bindable grObj hierarchy)
derive newtype instance Applicative (MakeGraphObject bindable grObj hierarchy)
derive newtype instance Monad (MakeGraphObject bindable grObj hierarchy)
derive newtype instance Bind (MakeGraphObject bindable grObj hierarchy)

-- | Helper functions to express the making of one step
-- of a graph object's visual tree, since their implementations
-- are all the same.
maker
  :: forall hierarchy bindable b p g
   . IsPanel p
  => IsGraphObject g
  => Effect g
  -> MakeGraphObject bindable g (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
maker constructor (MakeGraphObject howToMake) = MakeGraphObject $ do
  n <- liftEffect constructor
  made <- liftEffect $ runReaderT (howToMake *> ask) n
  parent <- ask
  _ <- liftEffect $ add_ made parent
  pure unit

makerPanel
  :: forall hierarchy bindable b p g
   . IsPanel p
  => IsPanel g
  => Effect g
  -> MakeGraphObject bindable g (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
makerPanel constructor (MakeGraphObject howToMake) = MakeGraphObject $ do
  n <- liftEffect constructor
  made <- liftEffect $ runReaderT (howToMake *> ask) n
  parent <- ask
  _ <- liftEffect $ add_ made parent
  pure unit

shape
  :: forall bindable p b hierarchy
   . IsPanel p
  => Figure
  -> MakeGraphObject bindable Shape_ (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
shape shapeType = maker (newShape $ show shapeType)

instance (GraphObjectAllFields grObj settable) => Bindable (MakeGraphObject bindable grObj hierarchy) bindable settable where
  binding' ptgt prsc go back = MakeGraphObject $ bindingImp bind_ ptgt prsc go back
  bindingOfObject' ptgt src go back = MakeGraphObject $ bindingOfObjectImp bind_ ptgt src go back

instance (IsGraphObject grObj) => AnimationTriggerable (MakeGraphObject bindable grObj hierarchy) where
  animationTrigger' p r startCondition = MakeGraphObject $ do
    grObj <- ask
    animTrigger <- liftEffect $ newAnimationTrigger'' (reflectSymbol p) (ffi r) (ffi startCondition)
    void $ liftEffect $ trigger_ animTrigger grObj

textBlock
  :: forall bindable p b hierarchy
   . IsPanel p
  => String
  -> MakeGraphObject bindable TextBlock_ (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
textBlock textBlockType = maker (newTextBlock textBlockType)

picture
  :: forall bindable p b hierarchy
   . IsPanel p
  => String
  -> MakeGraphObject bindable Picture_ (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
picture picSource = maker (newPicture picSource)

placeholder
  :: forall bindable p b hierarchy
   . IsPanel p
  => MakeGraphObject bindable Placeholder_ (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
placeholder = maker newPlaceholder

panel
  :: forall @panelType bindable p b hierarchy
   . IsPanel p
  => AsString panelType
  => MakeGraphObject bindable (PanelTypeTag panelType Panel_) (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
panel = makerPanel (PanelTypeTag <$> (newPanel $ asString @panelType))

button
  :: forall @buttonType @panelType bindable p b hierarchy
   . IsPanel p
  => AsString panelType
  => AsString buttonType
  => MakeGraphObject bindable (ButtonTypeTag buttonType panelType Button_) (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
button = makerPanel $ (ButtonTypeTag <$> newButton (asString @buttonType) (asString @panelType))

setter
  :: forall hierarchy bindable b g (singleton :: Row Type) (p :: Type) (@prop :: Symbol)
   . IsSymbol prop
  => IsPanel p
  => IsGraphObject g
  => Cons prop g () singleton
  => Effect g
  -> MakeGraphObject bindable g (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
setter constructor (MakeGraphObject howToMake) = MakeGraphObject $ do
  n <- liftEffect constructor
  made <- liftEffect $ runReaderT (howToMake *> ask) n
  parent <- ask
  _ <- liftEffect $ setUnsafe parent (insert (Proxy @prop) made {})
  pure unit

toolTip
  :: forall bindable p b hierarchy
   . IsPanel p
  => MakeGraphObject bindable (PanelTypeTag Auto' Adornment_) (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
toolTip = setter @"toolTip" (PanelTypeTag <$> newToolTip)

contextMenu
  :: forall bindable p b hierarchy
   . IsPanel p
  => MakeGraphObject bindable (PanelTypeTag Vertical' Adornment_) (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
contextMenu = setter @"contextMenu" (PanelTypeTag <$> newContextMenu)

-- | Helper function to express the making
-- of an entire graph object's visual tree.
mader
  :: forall bindable k b panelType
   . Effect k
  -> MakeGraphObject bindable (PanelTypeTag panelType k) Nil' b
  -> Made bindable k
mader constructor (MakeGraphObject howToMake) = Made $ do
  n <- constructor
  runReaderT (unPanelTypeTag <$> (howToMake *> ask)) (PanelTypeTag n)

node
  :: forall (@panelType :: PanelType) bindable b
   . AsString panelType
  => MakeGraphObject bindable (PanelTypeTag panelType Node_) Nil' b
  -> Made bindable Node_
node = mader ((newNode $ asString @panelType))

part
  :: forall @panelType bindable b
   . AsString panelType
  => MakeGraphObject bindable (PanelTypeTag panelType Part_) Nil' b
  -> Made bindable Part_
part = mader (newPart $ asString @panelType)

link
  :: forall bindable b
   . MakeGraphObject bindable (PanelTypeTag Link' Link_) Nil' b
  -> Made bindable Link_
link = mader newLink

group
  :: forall @panelType bindable b
   . AsString panelType
  => MakeGraphObject bindable (PanelTypeTag panelType Group_) Nil' b
  -> Made bindable Group_
group = mader (newGroup $ asString @panelType)

-- Groups can have their own layouts, so layouts can be constructed in the
-- context of making a group.
instance LayoutM (MakeGraphObject bindable (PanelTypeTag panelType Group_) rest) where
  layout constructor = MakeGraphObject <<< layoutImp constructor

instance
  ( GraphObjectAllFields g settable
  , GraphObjectChildFields g hierarchy childSettable
  , Union settable childSettable settable'
  ) =>
  Settable (MakeGraphObject bindable g hierarchy) settable' where
  set fields = MakeGraphObject $ setImp fields

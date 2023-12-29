module Went.Layout.Make where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Effect (Effect)
import Effect.Class (liftEffect)
import GoJS.Layout (class IsLayout, CircularLayout_, ForceDirectedLayout_, GridLayout_, LayeredDigraphLayout_, TreeLayout_, newCircularLayout, newForceDirectedLayout, newGridLayout, newLayeredDigraphLayout, newTreeLayout)
import GoJS.Settable (setUnsafe)
import Went.Layout.CircularLayout (CircularFields)
import Went.Layout.ForceDirectedLayout (ForceDirectedFields)
import Went.Layout.GridLayout (GridFields)
import Went.Layout.LayeredDigraphLayout (LayeredDigraphFields)
import Went.Layout.TreeLayout (TreeFields)
import Went.Settable (class Settable, setImp)

newtype MakeLayout (ffiType :: Type) (a :: Type) = MakeLayout (ReaderT ffiType Effect a)

derive newtype instance Functor (MakeLayout ffiType)
derive newtype instance Apply (MakeLayout ffiType)
derive newtype instance Applicative (MakeLayout ffiType)
derive newtype instance Bind (MakeLayout ffiType)
derive newtype instance Monad (MakeLayout ffiType)

-- | This class exists to support setting layouts in multiple monadic contexts
-- Both MakeDiagram and MakeGraphObject (Group_ :>>) support layouts. It also
-- makes this module export a more reasonable API by not requiring that each
-- implementer of the typeclass give all types of layouts.
class LayoutM (m :: Type -> Type) where
  layout :: forall l b. IsLayout l => Effect l -> (MakeLayout l b) -> m Unit

layoutImp :: forall (b :: Type) (l :: Type) (parent :: Type). Effect l -> MakeLayout l b -> ReaderT parent Effect Unit
layoutImp constructor (MakeLayout howToMakeLayout) = do
  l <- liftEffect constructor
  void $ liftEffect $ runReaderT howToMakeLayout l
  parent <- ask
  liftEffect $ setUnsafe parent {layout: l}

treeLayout :: forall (m :: Type -> Type) (b :: Type). LayoutM m => MakeLayout TreeLayout_ b -> m Unit
treeLayout = layout newTreeLayout
forceDirectedLayout :: forall (m :: Type -> Type) (b :: Type). LayoutM m => MakeLayout ForceDirectedLayout_ b -> m Unit
forceDirectedLayout = layout newForceDirectedLayout
gridLayout :: forall (m :: Type -> Type) (b :: Type). LayoutM m => MakeLayout GridLayout_ b -> m Unit
gridLayout = layout newGridLayout
circularLayout :: forall (m :: Type -> Type) (b :: Type). LayoutM m => MakeLayout CircularLayout_ b -> m Unit
circularLayout = layout newCircularLayout
layeredDigraphLayout :: forall (m :: Type -> Type) (b :: Type). LayoutM m => MakeLayout LayeredDigraphLayout_ b -> m Unit
layeredDigraphLayout = layout newLayeredDigraphLayout

class LayoutFields (ffiType :: Type) (fields :: Row Type) | ffiType -> fields

instance LayoutFields GridLayout_ GridFields
instance LayoutFields CircularLayout_ CircularFields
instance LayoutFields ForceDirectedLayout_ ForceDirectedFields
instance LayoutFields LayeredDigraphLayout_ LayeredDigraphFields
instance LayoutFields TreeLayout_ TreeFields

instance (LayoutFields ffiType settable) => Settable (MakeLayout ffiType) settable where
  set fields = MakeLayout $ setImp fields
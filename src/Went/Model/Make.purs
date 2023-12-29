module Went.Model.Make where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Effect (Effect)
import Effect.Class (liftEffect)
import GoJS.Model (class IsModel, GraphLinksModel_, Model_, TreeModel_, newGraphLinksModel, newModel, newTreeModel)
import GoJS.Settable (setUnsafe)
import Went.Model (ModelSpecificFields)
import Went.Model.GraphLinksModel (GraphLinksModelFields)
import Went.Model.TreeModel (TreeModelFields)
import Went.Settable (class Settable, setImp)

newtype MakeModel (ffiType :: Type) (nodeData :: Row Type) (linkData :: Row Type) (a :: Type) = MakeModel (ReaderT ffiType Effect a)

derive newtype instance Functor (MakeModel ffiType nodeData linkData)
derive newtype instance Apply (MakeModel ffiType nodeData linkData)
derive newtype instance Applicative (MakeModel ffiType nodeData linkData)
derive newtype instance Bind (MakeModel ffiType nodeData linkData)
derive newtype instance Monad (MakeModel ffiType nodeData linkData)

-- | This typeclass exists so that we may define model, treeModel and graphLinksModel
-- here instead of in the Diagram module.
class ModelM m nodeData linkData | m -> nodeData linkData where
  model' :: forall model b. IsModel model => Effect model -> (MakeModel model nodeData linkData b) -> m Unit

modelImp
  :: forall (modelType :: Type) parent nodeData linkData (b :: Type)
   . IsModel modelType
  => Effect modelType
  -> MakeModel modelType nodeData linkData b
  -> ReaderT parent Effect Unit
modelImp constructor (MakeModel howToMakeModel) = do
  l <- liftEffect constructor
  _ <- liftEffect $ runReaderT howToMakeModel l
  parent <- ask
  liftEffect $ setUnsafe parent { model: l }
  pure unit

model :: forall m nodeData linkData b. ModelM m nodeData linkData => MakeModel (Model_ nodeData) nodeData linkData b -> m Unit
model = model' newModel
treeModel :: forall m nodeData linkData b. ModelM m nodeData linkData => MakeModel (TreeModel_ nodeData) nodeData linkData b -> m Unit
treeModel = model' newTreeModel
graphLinksModel :: forall m nodeData linkData b. ModelM m nodeData linkData => MakeModel (GraphLinksModel_ nodeData linkData) nodeData linkData b -> m Unit
graphLinksModel = model' newGraphLinksModel

class ModelFields (ffiType :: Type) (fields :: Row Type) | ffiType -> fields

instance ModelFields (Model_ nodeData) (ModelSpecificFields (Model_ nodeData) nodeData ())
instance ModelFields (GraphLinksModel_ nodeData linkData) (GraphLinksModelFields nodeData linkData)
instance ModelFields (TreeModel_ nodeData) (TreeModelFields nodeData)

instance (ModelFields ffiType settable) => Settable (MakeModel ffiType nodeData linkData) settable where
  set fields = MakeModel $ setImp fields
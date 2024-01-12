module Went.Settable where

import Prelude

import Control.Monad.Reader (ReaderT, ask)
import Effect (Effect)
import Effect.Class (liftEffect)
import GoJS.Unsafe.Set (setUnsafe)
import Heterogeneous.Mapping (class HMap, hmap)
import Prim.Row (class Union)
import Went.FFI.Class (CreateFFIRecord(..))

class Monad m <= Settable (m :: Type -> Type) (superset :: Row Type) | m -> superset where
  set
    :: forall rIn rOut rest
     . HMap CreateFFIRecord (Record rIn) (Record rOut)
    => Union rIn rest superset
    => Record rIn
    -> m Unit

-- Every Settable instance looks the same, which is why we provide an implementation here.
-- The instances at the other modules are now free to import this implementation and use it,
-- and they no longer need to import HMap or FFIMap's CreateFFIRecord.
setImp
  :: forall r30 settable a
   . HMap CreateFFIRecord a (Record r30)
  => a
  -> ReaderT settable Effect Unit
setImp fields = do
  tbHandle <- ask
  liftEffect $ setUnsafe tbHandle (hmap CreateFFIRecord fields)
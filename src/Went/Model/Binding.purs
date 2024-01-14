module Went.Model.Binding where

import Prelude

import Control.Monad.Reader (ReaderT, ask)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect (Effect)
import Effect.Class (liftEffect)
import GoJS.Model.Binding.Constructors (newBinding)
import GoJS.Model.Binding.Methods (ofObject_)
import GoJS.Model.Types (Binding_)
import Prim.Row (class Cons)
import Type.Prelude (Proxy(..))
import Went.FFI.Class (class FFIMap)

-- | This typeclass exists because there's two concrete monads that call binding:
-- MakeGraphObject and MakeRowColumnDefinition.
class Bindable (m :: Type -> Type) (bindableFrom :: Row Type) (bindableTo :: Row Type) | m -> bindableFrom bindableTo where
  binding'
    :: forall (tgtProp :: Symbol) srcProp srcType tgtType srcTypeFFI tgtTypeFFI rto (rs :: Row Type)
     . IsSymbol tgtProp
    => IsSymbol srcProp
    => Cons tgtProp tgtType rto bindableTo
    => Cons srcProp srcType rs bindableFrom
    => FFIMap tgtType tgtTypeFFI
    => FFIMap srcType srcTypeFFI
    => Proxy tgtProp
    -> Proxy srcProp
    -> Maybe (srcTypeFFI -> tgtTypeFFI)
    -> Maybe (tgtTypeFFI -> srcTypeFFI)
    -> m Unit
  bindingOfObject'
    :: forall (tgtProp :: Symbol) tgtType srcTypeFFI tgtTypeFFI rto
     . IsSymbol tgtProp
    => Cons tgtProp tgtType rto bindableTo
    => FFIMap tgtType tgtTypeFFI
    => Proxy tgtProp
    -> String
    -> Maybe (srcTypeFFI -> tgtTypeFFI)
    -> Maybe (tgtTypeFFI -> srcTypeFFI)
    -> m Unit

-- | Convenience alias for class methods so that explicit Proxys don't have to be passed
binding
  :: forall (@tgtProp :: Symbol) (@srcProp :: Symbol) m srcType tgtType srcTypeFFI tgtTypeFFI rto rs bindableTo bindableFrom
   . IsSymbol tgtProp
  => IsSymbol srcProp
  => Cons tgtProp tgtType rto bindableTo
  => Cons srcProp srcType rs bindableFrom
  => FFIMap tgtType tgtTypeFFI -- TODO: Wtf?
  => FFIMap srcType srcTypeFFI
  => Bindable m bindableFrom bindableTo
  => Maybe (srcTypeFFI -> tgtTypeFFI)
  -> Maybe (tgtTypeFFI -> srcTypeFFI)
  -> m Unit
binding = binding' (Proxy @tgtProp) (Proxy @srcProp)

bindingOfObject
  :: forall (@tgtProp :: Symbol) tgtType m srcTypeFFI tgtTypeFFI rto bindableTo bindableFrom
   . IsSymbol tgtProp
  => Cons tgtProp tgtType rto bindableTo
  => FFIMap tgtType tgtTypeFFI
  => Bindable m bindableFrom bindableTo
  => String -- ^ Source property. This is given as a string because it can be a property of ANY graph object in a node's visual tree.
  -> Maybe (srcTypeFFI -> tgtTypeFFI)
  -> Maybe (tgtTypeFFI -> srcTypeFFI)
  -> m Unit
bindingOfObject = bindingOfObject' (Proxy @tgtProp)

binding1
  :: forall (@prop :: Symbol) m tgtType tgtTypeFFI rto rs bindableTo bindableFrom
   . IsSymbol prop
  => Cons prop tgtType rto bindableTo
  => Cons prop tgtType rs bindableFrom
  => FFIMap tgtType tgtTypeFFI
  => Bindable m bindableFrom bindableTo
  => m Unit
binding1 = binding @prop @prop Nothing Nothing

binding1TwoWay
  :: forall (@prop :: Symbol) m tgtType tgtTypeFFI rto rs bindableTo bindableFrom
   . IsSymbol prop
  => Cons prop tgtType rto bindableTo
  => Cons prop tgtType rs bindableFrom
  => FFIMap tgtType tgtTypeFFI
  => Bindable m bindableFrom bindableTo
  => m Unit
binding1TwoWay = binding @prop @prop (Just identity) (Just identity)

bindingImp
  :: forall a b from to tgt src
   . IsSymbol tgt
  => IsSymbol src
  => (Binding_ -> a -> Effect b)
  -> Proxy tgt
  -> Proxy src
  -> Maybe (from -> to)
  -> Maybe (to -> from)
  -> ReaderT a Effect b
bindingImp bindAct ptgt psrc go back = do
  handle <- ask
  nb <- liftEffect $ newBinding (reflectSymbol ptgt) (reflectSymbol psrc) go back
  liftEffect $ bindAct nb handle

bindingOfObjectImp
  :: forall a b from to tgt
   . IsSymbol tgt
  => (Binding_ -> a -> Effect b)
  -> Proxy tgt
  -> String
  -> Maybe (from -> to)
  -> Maybe (to -> from)
  -> ReaderT a Effect b
bindingOfObjectImp bindAct ptgt srcProp go back = do
  handle <- ask
  nb <- liftEffect $ newBinding (reflectSymbol ptgt) srcProp go back >>= ofObject_
  liftEffect $ bindAct nb handle
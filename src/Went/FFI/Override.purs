module Went.FFI.Override where

import Data.Function.Uncurried (Fn0, Fn1, Fn2)
import Effect (Effect)

newtype Override a b = Override (a -> b)

foreign import override2 :: forall this b c d. (this -> b -> c -> Effect d) -> Fn2 b c d
foreign import override1 :: forall this b c. (this -> b -> Effect c) -> Fn1 b c
foreign import override0 :: forall this c. (this -> Effect c) -> Fn0 c
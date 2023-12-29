module Went.FFI.Function where

import Data.Function.Uncurried (Fn0, Fn1, Fn2)
import Effect (Effect)

foreign import call2 :: forall a b c. (a -> b -> Effect c) -> Fn2 a b c
foreign import call1 :: forall a b. (a -> Effect b) -> Fn1 a b
foreign import call0 :: forall a. Effect a -> Fn0 a

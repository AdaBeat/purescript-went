module Went.Diagram.Animation.Easing where

import Data.Function.Uncurried (Fn4, runFn4)

foreign import easeLinear_ :: Fn4 Number Number Number Number Number

easeLinear :: EasingFn
easeLinear = runFn4 easeLinear_

foreign import easeInOutQuad_ :: Fn4 Number Number Number Number Number

easeInOutQuad :: EasingFn
easeInOutQuad = runFn4 easeInOutQuad_

foreign import easeInQuad_ :: Fn4 Number Number Number Number Number

easeInQuad :: EasingFn
easeInQuad = runFn4 easeInQuad_

foreign import easeOutQuad_ :: Fn4 Number Number Number Number Number

easeOutQuad :: EasingFn
easeOutQuad = runFn4 easeOutQuad_

foreign import easeInExpo_ :: Fn4 Number Number Number Number Number

easeInExpo :: EasingFn
easeInExpo = runFn4 easeInExpo_

foreign import easeOutExpo_ :: Fn4 Number Number Number Number Number

easeOutExpo :: EasingFn
easeOutExpo = runFn4 easeOutExpo_

type EasingFn = Number -> Number -> Number -> Number -> Number

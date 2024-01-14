module Went.Diagram.Animation.Easing where

import Data.Function.Uncurried (Fn4)

foreign import easeLinear :: Fn4 Number Number Number Number Number
foreign import easeInOutQuad :: Fn4 Number Number Number Number Number
foreign import easeInQuad :: Fn4 Number Number Number Number Number 
foreign import easeOutQuad :: Fn4 Number Number Number Number Number
foreign import easeInExpo :: Fn4 Number Number Number Number Number 
foreign import easeOutExpo :: Fn4 Number Number Number Number Number
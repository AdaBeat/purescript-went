module Went.Diagram.Animation.AnimationTrigger where

import Prelude

import Data.Symbol (class IsSymbol)
import Effect (Effect)
import GoJS.Diagram (Animation_)
import Prim.Row (class Cons, class Union)
import Type.Prelude (Proxy(..))
import Went.Diagram.Animation.Easing (EasingFn)
import Went.Diagram.Animation.EnumValue.StartCondition (StartCondition)
import Went.FFI.Class (class FFIMap)
import Went.Geometry.Point (Point)
import Went.Geometry.Size (Size)

type AnimatableGraphObjectAllFields =
  ( position :: Point
  , location :: Point -- (on Parts)
  , scale :: Number --
  , opacity :: Number --
  , angle :: Number --
  , desiredSize :: Size --
  , width :: Number --
  , height :: Number --
  , background :: String --
  , fill :: String -- (on Shapes)
  , strokeWidth :: Number -- (on Shapes)
  , strokeDashOffset :: Number -- (on Shapes)
  , stroke :: String -- (on Shapes, TextBlocks)
  )

type FinishedFn = Animation_ -> Effect Unit
type AnimationSettings =
  ( duration :: Number
  , easing :: EasingFn
  , finished :: FinishedFn
  )

-- { duration?: number; easing?: EasingFunction; finished?: any }
class AnimationTriggerable (m :: Type -> Type) where
  animationTrigger'
    :: forall (tgtProp :: Symbol) tgtType rto (rs :: Row Type) as asFFI ras
     . IsSymbol tgtProp
    => Cons tgtProp tgtType rto AnimatableGraphObjectAllFields
    => Union as ras AnimationSettings
    => FFIMap (Record as) (Record asFFI)
    => Proxy tgtProp
    -> Record as
    -> StartCondition
    -> m Unit

animationTrigger
  :: forall (@tgtProp :: Symbol) (tgtType :: Type) rto (rs :: Row Type) m as asFFI ras
   . IsSymbol tgtProp
  => Cons tgtProp tgtType rto AnimatableGraphObjectAllFields
  => Union as ras AnimationSettings
  => FFIMap (Record as) (Record asFFI)
  => AnimationTriggerable m
  => Record as
  -> StartCondition
  -> m Unit
animationTrigger = animationTrigger' (Proxy @tgtProp)

module Went.Diagram.Animation.Make where

import Prelude

import Control.Monad.Reader (ReaderT, ask, asks)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), snd)
import Data.Variant (inj)
import Effect (Effect)
import Effect.Class (liftEffect)
import GoJS.Diagram (Animation_)
import GoJS.Diagram.Animation.Methods (add_, start_)
import GoJS.Diagram.Types (Diagram_)
import GoJS.GraphObject.Types (class IsGraphObject, GraphObject_)
import Prim.Row (class Cons)
import Type.Prelude (Proxy(..))
import Went.Diagram.Animation.PlainFields (AnimationFields)
import Went.FFI.Class (class FFIMap, ffi)
import Went.Geometry.Point (Point(..))
import Went.Geometry.Size (Size)
import Went.Settable (class Settable, set, setImp)

newtype MakeAnimation obj b = MakeAnimation (ReaderT (Tuple obj Animation_) Effect b)

derive newtype instance Functor (MakeAnimation obj)
derive newtype instance Apply (MakeAnimation obj)
derive newtype instance Applicative (MakeAnimation obj)
derive newtype instance Monad (MakeAnimation obj)
derive newtype instance Bind (MakeAnimation obj)

type AnimatableGraphObjectFields =
  (
    position :: Point
  , location :: Point -- (on Parts)
  , scale :: Number --
  , opacity :: Number --
  , angle :: Number --
  , desiredSize :: Size --
  , width :: Number --
  , height :: Number  --
  , background :: String --
  , fill :: String -- (on Shapes)
  , strokeWidth :: Number -- (on Shapes)
  , strokeDashOffset :: Number -- (on Shapes)
  , stroke :: String ) -- (on Shapes, TextBlocks)

instance Settable (MakeAnimation obj) AnimationFields where
  set = MakeAnimation <<< setImp
-- animation.add(node, "position", node.position, new go.Point(400, 500));

-- add
--   :: forall startVal endVal obj @tgtProp rt startValFFI endValFFI
--    . IsGraphObject obj
--   => IsSymbol tgtProp
--   => Cons tgtProp startVal rt AnimatableGraphObjectFields
--   => Cons tgtProp endVal rt AnimatableGraphObjectFields
--   => FFIMap startVal startValFFI
--   => FFIMap endVal endValFFI
--   => startVal
--   -> endVal
--   -> Boolean
--   -> MakeAnimation obj Unit
-- add startVal endVal isStyle = MakeAnimation $ do
--   Tuple obj animation <- ask
--   _ <- liftEffect $ add_ @GraphObject_ @Diagram_ (inj (Proxy @"graphObject") obj) (reflectSymbol (Proxy @tgtProp)) (ffi startVal) (ffi endVal) isStyle animation
--   pure unit

start :: forall obj. IsGraphObject obj => MakeAnimation obj Unit
start = MakeAnimation $
  void $ asks snd >>= liftEffect <<< start_

-- anim = do
--   add @"position" (Point {x: 0.0, y: 1.0}) (Point {x: 2.0, y: 2.0}) false
--   set {duration: 1000.0}
--   start


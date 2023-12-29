module Went.GraphObject.Shape.MonadicFields where

import Prelude

import Effect.Exception.Unsafe (unsafeThrow)
import GoJS.GraphObject.Types (class IsGraphObject, Shape_)
import Type.Data.List (Nil')
import Went.GraphObject.Make (MakeGraphObject(..))

-- TODO: Implementing this depends on determining that the behavior of a
-- MakeGraphObject with no hierarchy has no parent to add it to.
pathPattern
  :: forall bindable g hierarchy
   . IsGraphObject g
  => MakeGraphObject bindable Shape_ Nil' g
  -> MakeGraphObject bindable Shape_ hierarchy Unit
pathPattern (MakeGraphObject _) = unsafeThrow ""
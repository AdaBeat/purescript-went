module Went.Diagram.Event where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data DiagramEvent
  = ChangedSelection
  | Modified
  | LinkDrawn
  | LinkRelinked

derive instance Generic DiagramEvent _
instance Show DiagramEvent where
  show = genericShow


module Went.GraphObject.TextBlock.TextAlign where

import Prelude

data TextAlign
  = Left
  | Right
  | Center
  | End
  | Start
instance Show TextAlign where
  show = case _ of
    Left -> "left"
    Right -> "right"
    Center -> "center"
    End -> "end"
    Start -> "start"
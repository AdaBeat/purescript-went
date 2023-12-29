module Went.Geometry.Margin where

data Margin
  = MarginEach
      { bottom :: Number
      , left :: Number
      , right :: Number
      , top :: Number
      }
  | MarginAll Number

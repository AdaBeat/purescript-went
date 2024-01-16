module Went.GraphObject.Panel.AsString where

-- | Helper class to convert "singleton" types to strings
class AsString :: forall k. k -> Constraint
class AsString p where
  asString :: String

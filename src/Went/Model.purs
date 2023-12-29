module Went.Model where

type ModelSpecificFields (this :: Type) (nodeData :: Row Type) (r :: Row Type) =
  ( copiesArrayObjects :: Boolean
  , copiesArrays :: Boolean
  , copiesKey :: Boolean
  , copyNodeDataFunction :: Record nodeData -> this -> Record nodeData
  , dataFormat :: String
  , isReadOnly :: Boolean
  -- TODO:  missing: makeUniqueKeyFunction, modelData, nodeKeyProperty, undoManager
  , name :: String
  , nodeDataArray :: Array (Record nodeData)
  , skipsUndoManager :: Boolean
  | r
  )

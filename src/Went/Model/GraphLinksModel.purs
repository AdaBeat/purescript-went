module Went.Model.GraphLinksModel where

import GoJS.Model.Types (GraphLinksModel_)
import Went.Model (ModelSpecificFields)

type GraphLinksModelSpecificFields nodeData linkData =
  ( archetypeNodeData :: Record nodeData
  , copyLinkData :: Record linkData -> GraphLinksModel_ nodeData linkData -> Record linkData
  , linkDataArray :: Array (Record linkData)
  -- TODO: Missing: These should support functions to keys as well
  , linkCategoryProperty :: String
  , linkFromKeyProperty :: String
  , linkFromPortIdProperty :: String
  , linkKeyProperty :: String
  , linkLabelKeysProperty :: String
  , linkToKeyProperty :: String
  , linkToPortIdProperty :: String
  -- TODO: Missing: makeUniqueLinkKeyFunction, nodeGroupKeyProperty, nodeIsGroupProperty
  )

type GraphLinksModelFields nodeData linkData = ModelSpecificFields (GraphLinksModel_ nodeData linkData) nodeData (GraphLinksModelSpecificFields nodeData linkData)
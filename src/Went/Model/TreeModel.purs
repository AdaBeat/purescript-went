module Went.Model.TreeModel where

import GoJS.Model.Types (TreeModel_)
import Went.Model (ModelSpecificFields)

type TreeModelSpecificFields =
  ( nodeParentKeyProperty :: String
  , parentLinkCategoryProperty :: String
  )

type TreeModelFields nodeData = ModelSpecificFields (TreeModel_ nodeData) nodeData TreeModelSpecificFields
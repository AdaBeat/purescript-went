module Went.Model.TreeModel where

import GoJS.Model.Types (TreeModel_)
import GoJS.Key
import Went.Model (ModelSpecificFields)

type TreeModelSpecificFields nodeData =
  ( nodeParentKeyProperty :: KeyProperty nodeData Key
  , parentLinkCategoryProperty :: KeyProperty nodeData String
  )

type TreeModelFields nodeData = ModelSpecificFields (TreeModel_ nodeData) nodeData (TreeModelSpecificFields nodeData)
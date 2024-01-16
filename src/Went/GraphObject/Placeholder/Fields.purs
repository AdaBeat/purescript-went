module Went.GraphObject.Placeholder.Fields where

import GoJS.GraphObject.Types (Placeholder_)

import Went.Geometry.Margin (Margin)
import Went.GraphObject.Fields.Specific (GraphObjectSpecificFields)


type PlaceholderSpecificFields =
  ( padding :: Margin
  )

type PlaceholderFields = GraphObjectSpecificFields Placeholder_ PlaceholderSpecificFields

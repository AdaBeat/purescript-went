module Went.GraphObject.Placeholder where

import GoJS.GraphObject.Types (Placeholder_)

import Went.Geometry.Margin (Margin)
import Went.GraphObject (GraphObjectSpecificFields)


type PlaceholderSpecificFields =
  ( padding :: Margin
  )

type PlaceholderFields = GraphObjectSpecificFields Placeholder_ PlaceholderSpecificFields

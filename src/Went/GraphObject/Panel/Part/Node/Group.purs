module Went.GraphObject.Panel.Part.Node.Group where

import Prelude

import Effect (Effect)
import GoJS.GraphObject.Types (Group_, Part_)
import Went.GraphObject (GraphObjectSpecificFields)
import Went.GraphObject.Panel (PanelSpecificFields)
import Went.GraphObject.Panel.Part (PartSpecificFields)
import Went.GraphObject.Panel.Part.Node (NodeSpecificFields)

{-
computesBoundsAfterDrag
computesBoundsIncludingLinks
computesBoundsIncludingLocation
handlesDragDropForMembers
isSubGraphExpanded
layout - Monadic
memberAdded
memberParts - Read-only
memberRemoved
memberValidation
placeholder - Read-only
subGraphExpandedChanged
ungroupable
wasSubGraphExpanded
-}

type GroupSpecificFields (a :: Row Type) =
  ( computesBoundsAfterDrag :: Boolean
  , computesBoundsIncludingLinks :: Boolean
  , computesBoundsIncludingLocation :: Boolean
  , handlesDragDropForMembers :: Boolean
  , isSubGraphExpanded :: Boolean
  , memberAdded :: Group_ -> Part_ -> Effect Unit
  , memberRemoved :: Group_ -> Part_ -> Effect Unit
  , memberValidation :: Group_ -> Part_ -> Boolean
  , subGraphExpandedChanged :: Group_ -> Effect Unit
  , ungroupable :: Boolean
  , wasSubGraphExpanded :: Boolean
  | a
  )

type GroupFields (extraFields :: Row Type) =
  GraphObjectSpecificFields Group_
    ( PanelSpecificFields
        ( PartSpecificFields Group_
            ( NodeSpecificFields Group_
                ( GroupSpecificFields extraFields
                )
            )
        )
    )

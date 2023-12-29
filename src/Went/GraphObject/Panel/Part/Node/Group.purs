module Went.GraphObject.Panel.Part.Node.Group where

import Prelude

import Effect (Effect)
import GoJS.GraphObject.Types (Group_, SomeNode_, SomePart_)
import Went.GraphObject (GraphObjectSpecificFields)
import Went.GraphObject.Panel (PanelSpecificFields)
import Went.GraphObject.Panel.Part (PartSpecificFields)
import Went.GraphObject.Panel.Part.Node (NodeSpecificFields)

type GroupSpecificFields (a :: Row Type) =
  ( computesBoundsAfterDrag :: Boolean
  , computesBoundsIncludingLinks :: Boolean
  , computesBoundsIncludingLocation :: Boolean
  , handlesDragDropForMembers :: Boolean
  , isSubGraphExpanded :: Boolean
  , memberAdded :: SomeNode_ -> SomePart_ -> Effect Unit
  , memberRemoved :: SomeNode_ -> SomePart_ -> Effect Unit
  , memberValidation :: SomeNode_ -> SomePart_ -> Boolean
  , subGraphExpandedChanged :: SomeNode_ -> Effect Unit
  , ungroupable :: Boolean
  , wasSubGraphExpanded :: Boolean
  | a
  )

type GroupFields (extraFields :: Row Type) =
  GraphObjectSpecificFields Group_
    ( PanelSpecificFields
        ( PartSpecificFields
            ( NodeSpecificFields
                ( GroupSpecificFields extraFields
                )
            )
        )
    )
-- layout :: MadeLayout l

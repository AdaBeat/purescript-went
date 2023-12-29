module Went.GraphObject.Panel.Part.Node where

import Prelude

import Effect (Effect)
import GoJS.GraphObject.Types (Link_, Node_, SomeGraphObject_, SomeNode_)
import Went.Geometry.Margin (Margin)
import Went.GraphObject (GraphObjectSpecificFields)
import Went.GraphObject.EnumValue.PortSpreading (PortSpreading)
import Went.GraphObject.Panel (PanelSpecificFields)
import Went.GraphObject.Panel.Part (PartSpecificFields)

type NodeSpecificFields (a :: Row Type) =
  ( avoidable :: Boolean
  , avoidableMargin :: Margin
  , isTreeExpanded :: Boolean
  , isTreeLeaf :: Boolean
  , linkConnected :: SomeNode_ -> Link_ -> SomeGraphObject_ -> Effect Unit
  , linkDisconnected :: SomeNode_ -> Link_ -> SomeGraphObject_ -> Effect Unit
  , linkValidation :: SomeNode_ -> SomeGraphObject_ -> SomeNode_ -> SomeGraphObject_ -> Link_ -> Effect Boolean
  , portSpreading :: PortSpreading
  , treeExpandedChanged :: SomeNode_ -> Effect Unit
  , wasTreeExpanded :: Boolean
  | a
  )

-- labeledLink, linksConnected

type NodeFields (extraFields :: Row Type) =
  GraphObjectSpecificFields Node_
    ( PanelSpecificFields
        ( PartSpecificFields
            ( NodeSpecificFields extraFields
            )
        )
    )

module Went.GraphObject.Panel.Part.Node where

import Prelude

import Effect (Effect)
import GoJS.GraphObject.Types (Link_, Node_, GraphObject_)
import Went.Geometry.Margin (Margin)
import Went.GraphObject (GraphObjectSpecificFields)
import Went.GraphObject.EnumValue.PortSpreading (PortSpreading)
import Went.GraphObject.Panel (PanelSpecificFields)
import Went.GraphObject.Panel.Part (PartSpecificFields)


{-
avoidable
avoidableMargin
isLinkLabel - Read-only
isTreeExpanded 
isTreeLeaf
labeledLink - Read-only, in a way
linkConnected 
linkDisconnected
linkValidation
linksConnected - Read-only
port - Read-only
portSpreading
ports - Read-only
treeExpandedChanged
wasTreeExpanded
-}

type NodeSpecificFields (this :: Type) (a :: Row Type) =
  ( avoidable :: Boolean
  , avoidableMargin :: Margin
  , isTreeExpanded :: Boolean
  , isTreeLeaf :: Boolean
  , linkConnected :: this -> Link_ -> GraphObject_ -> Effect Unit
  , linkDisconnected :: this -> Link_ -> GraphObject_ -> Effect Unit
  , linkValidation :: this -> GraphObject_ -> Node_ -> GraphObject_ -> Link_ -> Effect Boolean
  , portSpreading :: PortSpreading
  , treeExpandedChanged :: this -> Effect Unit
  , wasTreeExpanded :: Boolean
  | a
  )

-- labeledLink, linksConnected

type NodeFields (this :: Type) (extraFields :: Row Type) =
  GraphObjectSpecificFields Node_
    ( PanelSpecificFields
        ( PartSpecificFields this
            ( NodeSpecificFields this extraFields
            )
        )
    )

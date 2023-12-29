module Went.GraphObject.Picture where

import Prelude

import Effect (Effect)
import GoJS.GraphObject.Types (Picture_)
import Went.Geometry.Rect (Rect)
import Went.Geometry.Spot (Spot)
import Went.GraphObject (GraphObjectSpecificFields)
import Went.GraphObject.EnumValue.Flip (Flip)
import Went.GraphObject.EnumValue.Stretch (Stretch)

type PictureSpecificFields =
  -- TODO: String is stand-in for generic undocumented Event type
  ( errorFunction :: Picture_ -> String -> Effect Unit
  , flip :: Flip
  , imageAlignment :: Spot
  , imageStretch :: Stretch
  , source :: String
  , sourceCrossOrigin :: Picture_ -> String
  , sourceRect :: Rect
  , successFunction :: Picture_ -> String -> Effect Unit
  )

type PictureFields = GraphObjectSpecificFields Picture_ PictureSpecificFields

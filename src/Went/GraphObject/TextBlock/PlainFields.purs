module Went.GraphObject.TextBlock.PlainFields where

import Prelude

import Effect (Effect)
import GoJS.GraphObject.Types (TextBlock_)
import GoJS.Tool.Types (TextEditingTool_)
import Went.Geometry.Margin (Margin)
import Went.Geometry.Spot (Spot)
import Went.GraphObject (GraphObjectSpecificFields)
import Went.GraphObject.EnumValue.Flip (Flip)
import Went.GraphObject.EnumValue.Formatting (Formatting)
import Went.GraphObject.EnumValue.Overflow (Overflow)
import Went.GraphObject.EnumValue.Wrap (Wrap)
import Went.GraphObject.TextBlock.TextAlign (TextAlign)

type TextBlockSpecificFields =
  ( choices :: Array String
  , editable :: Boolean
  , errorFunction :: TextEditingTool_ -> String -> String -> Effect Unit
  , flip :: Flip
  , formatting :: Formatting
  , font :: String
  , isMultiline :: Boolean
  , isStrikethrough :: Boolean
  , isUnderline :: Boolean
  , maxLines :: Int
  , overflow :: Overflow
  , margin :: Margin
  , spacingAbove :: Number
  , spacingBelow :: Number
  , stroke :: String
  , text :: String
  , textAlign :: TextAlign
  , textEdited :: TextBlock_ -> String -> String -> Effect Unit
  -- TODO: textEditor
  , textValidation :: TextBlock_ -> String -> String -> Boolean
  , verticalAlignment :: Spot
  , wrap :: Wrap
  -- Fields that only make sense when this TextBlock is in a Graduated panel.
  -- graduatedEnd :: Number
  -- graduatedFunction :: Number -> TextBlock_ -> String
  -- graduatedSkip :: Number -> TextBlock_ -> Boolean
  -- graduatedStart :: Number
  -- interval :: Number
  )

type TextBlockFields = GraphObjectSpecificFields TextBlock_ TextBlockSpecificFields

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

{-
choices
editable
errorFunction
flip
font
formatting
graduatedEnd -- Only when in Graduated Panel
graduatedFunction -- Only when in Graduated Panel
graduatedSkip -- Only when in Graduated Panel
graduatedStart -- Only when in Graduated Panel
interval -- Only when in Grid or Graduated Panel
isMultiline
isOverflowed -- Read-only
isStrikethrough
isUnderline
lineCount -- Read-only
lineHeight -- Read-only
maxLines
naturalBounds -- Read-only
overflow
spacingAbove
spacingBelow
stroke
text
textAlign
textEdited
textEditor -- Monadic
textValidation
verticalAlignment
wrap
-}

type TextBlockSpecificFields =
  ( choices :: Array String
  , editable :: Boolean
  , errorFunction :: TextEditingTool_ -> String -> String -> Effect Unit
  , flip :: Flip
  , font :: String
  , formatting :: Formatting
  , isMultiline :: Boolean
  , isStrikethrough :: Boolean
  , isUnderline :: Boolean
  , margin :: Margin
  , maxLines :: Int
  , overflow :: Overflow
  , spacingAbove :: Number
  , spacingBelow :: Number
  , stroke :: String
  , text :: String
  , textAlign :: TextAlign
  , textEdited :: TextBlock_ -> String -> String -> Effect Unit
  , textValidation :: TextBlock_ -> String -> String -> Boolean
  , verticalAlignment :: Spot
  , wrap :: Wrap
  )

type TextBlockFields = GraphObjectSpecificFields TextBlock_ TextBlockSpecificFields

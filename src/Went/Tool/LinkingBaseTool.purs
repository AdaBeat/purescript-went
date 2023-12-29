module Went.Tool.LinkingBaseTool where

type LinkingBaseToolSpecificFields r =
  ( isForwards :: Boolean
  , isUnconnectedLinkValid :: Boolean
  , linkingCursor :: String
  -- TODO: A lot is missing. The Tool rework will take a lot of thought
  | r
  )

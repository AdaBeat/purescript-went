module Went.Layout.ForceDirectedLayout where

import GoJS.Layout.Types (ForceDirectedLayout_, ForceDirectedNetwork_)
import Went.Geometry.Size (Size)
import Went.Layout (LayoutSpecificFields)

type ForceDirectedSpecificFields =
  ( arrangementSpacing :: Size
  , arrangesToOrigin :: Boolean
  , comments :: Boolean
  , defaultCommentElectricalCharge :: Number
  , defaultCommentSpringLength :: Number
  , defaultElectricalCharge :: Number
  , defaultGravitationalMass :: Number
  , defaultSpringLength :: Number
  , defaultSpringStiffness :: Number
  , epsilonDistance :: Number
  , infinityDistance :: Number
  , maxIterations :: Int
  , moveLimit :: Number
  -- Missing: randomNumberGenerator, currentIteration (read-only)
  , setsPortSpots :: Boolean
  )

type ForceDirectedFields = LayoutSpecificFields ForceDirectedLayout_ ForceDirectedNetwork_ ForceDirectedSpecificFields

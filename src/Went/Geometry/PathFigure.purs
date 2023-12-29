module Went.Geometry.PathFigure
  ( PathFigure
  , pathFigure
  , unPathFigure
  ) where

import Prelude

import Went.Geometry.PathSegment (PathSegment)
import Prim.Row (class Nub, class Union)
import Record.Builder (build, merge)

type PathFigureFields =
  ( sx :: Number
  , sy :: Number
  , filled :: Boolean
  , shadowed :: Boolean
  , isEvenOdd :: Boolean
  , segments :: Array PathSegment
  )

newtype PathFigure = PathFigure (Record PathFigureFields)

default :: Record PathFigureFields
default = { sx: 0.0, sy: 0.0, filled: true, shadowed: true, isEvenOdd: false, segments: [] }

pathFigure
  :: forall subset total
   . Union subset PathFigureFields total
  => Nub total PathFigureFields
  => Record subset
  -> PathFigure
pathFigure fields = PathFigure $ build (merge fields) default

unPathFigure :: PathFigure -> Record PathFigureFields
unPathFigure (PathFigure fields) = fields
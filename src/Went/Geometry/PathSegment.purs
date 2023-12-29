module Went.Geometry.PathSegment where

data PathSegment
  = Line Number Number
  | QuadraticBezier Number Number Number Number
  | Bezier Number Number Number Number Number Number
  | Arc Number Number Number Number Number Number
  | SvgArc Number Number Number Number Number Number Boolean
  | Move Number Number
  | Close PathSegment
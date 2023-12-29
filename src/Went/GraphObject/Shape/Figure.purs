module Went.GraphObject.Shape.Figure where

import Prelude

data Figure 
  = Rectangle
  | Square
  | RoundedRectangle
  | Border
  | Ellipse
  | Circle
  | TriangleRight
  | TriangleDown
  | TriangleLeft
  | TriangleUp
  | Triangle
  | Diamond
  | LineH
  | LineV
  | BarH
  | BarV
  | MinusLine
  | PlusLine
  | XLine
  | LineRight
  | LineDown
  | LineLeft
  | LineUp
  | None
  | Custom String
instance Show Figure where
  show = case _ of
    Rectangle -> "Rectangle"
    Square -> "Square"
    RoundedRectangle -> "RoundedRectangle"
    Border -> "Border"
    Ellipse -> "Ellipse"
    Circle -> "Circle"
    TriangleRight -> "TriangleRight"
    TriangleDown -> "TriangleDown"
    TriangleLeft -> "TriangleLeft"
    TriangleUp -> "TriangleUp"
    Triangle -> "Triangle"
    Diamond -> "Diamond"
    LineH -> "LineH"
    LineV -> "LineV"
    BarH -> "BarH"
    BarV -> "BarV"
    MinusLine -> "MinusLine"
    PlusLine -> "PlusLine"
    XLine -> "XLine"
    LineRight -> "LineRight"
    LineDown -> "LineDown"
    LineLeft -> "LineLeft"
    LineUp -> "LineUp"
    None -> "None"
    Custom name -> name
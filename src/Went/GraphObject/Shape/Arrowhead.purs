module Went.GraphObject.Shape.Arrowhead where

import Prelude
data Arrowhead
 = Standard
 | AccelerationArrow
 | BackSlash
 | Backward
 | BackwardBoomerang
 | BackwardCircleFork
 | BackwardCircleLineFork
 | BackwardDoubleFeathers
 | BackwardFeather
 | BackwardFork
 | BackwardHalfTriangleBottom
 | BackwardHalfTriangleTop
 | BackwardLineFork
 | BackwardOpenTriangle
 | BackwardOpenTriangleBottom
 | BackwardOpenTriangleLine
 | BackwardOpenTriangleTop
 | BackwardSemiCircle
 | BackwardTriangle
 | BackwardTripleFeathers
 | BackwardV
 | BigEndArrow
 | Block
 | Boomerang
 | BoxArrow
 | Chevron
 | Circle
 | CircleEndedArrow
 | CircleFork
 | CircleLine
 | CircleLineFork
 | ConcaveTailArrow
 | Diamond
 | DiamondCircle
 | DoubleBackSlash
 | DoubleFeathers
 | DoubleForwardSlash
 | DoubleLine
 | DoubleLineCircle
 | DoubleTriangle
 | DynamicWidthArrow
 | EquilibriumArrow
 | FastForward
 | Feather
 | Fork
 | ForwardSemiCircle
 | ForwardSlash
 | HalfArrowBottom
 | HalfArrowTop
 | HalfTriangleBottom
 | HalfTriangleTop
 | Kite
 | Line
 | LineCircle
 | LineFork
 | NormalArrow
 | OpenRightTriangleBottom
 | OpenRightTriangleTop
 | OpenTriangle
 | OpenTriangleBottom
 | OpenTriangleLine
 | OpenTriangleTop
 | OpposingDirectionDoubleArrow
 | PartialDoubleTriangle
 | PentagonArrow
 | PlusCircle
 | RoundedTriangle
 | SidewaysV
 | SimpleArrow
 | StretchedChevron
 | StretchedDiamond
 | TailedNormalArrow
 | Triangle
 | TriangleLine
 | TripleBackSlash
 | TripleFeathers
 | TripleForwardSlash
 | TripleLine
 | TripleLineCircle
 | X
 | Custom String

instance Show Arrowhead where
  show = case _ of
    Standard -> "Standard"
    AccelerationArrow -> "AccelerationArrow"
    BackSlash -> "BackSlash"
    Backward -> "Backward"
    BackwardBoomerang -> "BackwardBoomerang"
    BackwardCircleFork -> "BackwardCircleFork"
    BackwardCircleLineFork -> "BackwardCircleLineFork"
    BackwardDoubleFeathers -> "BackwardDoubleFeathers"
    BackwardFeather -> "BackwardFeather"
    BackwardFork -> "BackwardFork"
    BackwardHalfTriangleBottom -> "BackwardHalfTriangleBottom"
    BackwardHalfTriangleTop -> "BackwardHalfTriangleTop"
    BackwardLineFork -> "BackwardLineFork"
    BackwardOpenTriangle -> "BackwardOpenTriangle"
    BackwardOpenTriangleBottom -> "BackwardOpenTriangleBottom"
    BackwardOpenTriangleLine -> "BackwardOpenTriangleLine"
    BackwardOpenTriangleTop -> "BackwardOpenTriangleTop"
    BackwardSemiCircle -> "BackwardSemiCircle"
    BackwardTriangle -> "BackwardTriangle"
    BackwardTripleFeathers -> "BackwardTripleFeathers"
    BackwardV -> "BackwardV"
    BigEndArrow -> "BigEndArrow"
    Block -> "Block"
    Boomerang -> "Boomerang"
    BoxArrow -> "BoxArrow"
    Chevron -> "Chevron"
    Circle -> "Circle"
    CircleEndedArrow -> "CircleEndedArrow"
    CircleFork -> "CircleFork"
    CircleLine -> "CircleLine"
    CircleLineFork -> "CircleLineFork"
    ConcaveTailArrow -> "ConcaveTailArrow"
    Diamond -> "Diamond"
    DiamondCircle -> "DiamondCircle"
    DoubleBackSlash -> "DoubleBackSlash"
    DoubleFeathers -> "DoubleFeathers"
    DoubleForwardSlash -> "DoubleForwardSlash"
    DoubleLine -> "DoubleLine"
    DoubleLineCircle -> "DoubleLineCircle"
    DoubleTriangle -> "DoubleTriangle"
    DynamicWidthArrow -> "DynamicWidthArrow"
    EquilibriumArrow -> "EquilibriumArrow"
    FastForward -> "FastForward"
    Feather -> "Feather"
    Fork -> "Fork"
    ForwardSemiCircle -> "ForwardSemiCircle"
    ForwardSlash -> "ForwardSlash"
    HalfArrowBottom -> "HalfArrowBottom"
    HalfArrowTop -> "HalfArrowTop"
    HalfTriangleBottom -> "HalfTriangleBottom"
    HalfTriangleTop -> "HalfTriangleTop"
    Kite -> "Kite"
    Line -> "Line"
    LineCircle -> "LineCircle"
    LineFork -> "LineFork"
    NormalArrow -> "NormalArrow"
    OpenRightTriangleBottom -> "OpenRightTriangleBottom"
    OpenRightTriangleTop -> "OpenRightTriangleTop"
    OpenTriangle -> "OpenTriangle"
    OpenTriangleBottom -> "OpenTriangleBottom"
    OpenTriangleLine -> "OpenTriangleLine"
    OpenTriangleTop -> "OpenTriangleTop"
    OpposingDirectionDoubleArrow -> "OpposingDirectionDoubleArrow"
    PartialDoubleTriangle -> "PartialDoubleTriangle"
    PentagonArrow -> "PentagonArrow"
    PlusCircle -> "PlusCircle"
    RoundedTriangle -> "RoundedTriangle"
    SidewaysV -> "SidewaysV"
    SimpleArrow -> "SimpleArrow"
    StretchedChevron -> "StretchedChevron"
    StretchedDiamond -> "StretchedDiamond"
    TailedNormalArrow -> "TailedNormalArrow"
    Triangle -> "Triangle"
    TriangleLine -> "TriangleLine"
    TripleBackSlash -> "TripleBackSlash"
    TripleFeathers -> "TripleFeathers"
    TripleForwardSlash -> "TripleForwardSlash"
    TripleLine -> "TripleLine"
    TripleLineCircle -> "TripleLineCircle"
    X -> "X"
    Custom s -> s

module Went.FFI.Class where

import Prelude

import Data.Function.Uncurried (Fn0, Fn1, Fn2, Fn3, Fn4, mkFn1, mkFn2, mkFn3)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import GoJS.Debug (trace)
import GoJS.EnumValue (EnumValue_)
import GoJS.Geometry.Constructors (newGeometry)
import GoJS.Geometry.Margin.Constructors (newMargin)
import GoJS.Geometry.PathFigure.Constructors (pathFigure_)
import GoJS.Geometry.PathSegment.Constructors (pathSegmentLine_, pathSegmentQuadraticBezier_, pathSegmentBezier_, pathSegmentArc_, pathSegmentSvgArc_, pathSegmentMove_, close_)
import GoJS.Geometry.Point.Constructors (newPoint)
import GoJS.Geometry.Rect.Constructors (newRect)
import GoJS.Geometry.Size.Constructors (newSize)
import GoJS.Geometry.Spot (bottomLeftSides_, bottomRightSides_, bottomSide_, leftRightSides_, leftSide_, newSpot, rightSide_, topBottomSides_, topLeftSides_, topRightSides_, topSide_)
import GoJS.Geometry.Types (Geometry_, Margin_, PathFigure_, PathSegment_, Point_, Rect_, Size_, Spot_)
import GoJS.Key (Key(..), KeyProperty(..))
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Unsafe.Coerce (unsafeCoerce)
import Went.EnumValue (class EnumValueFFI, enumValue)
import Went.FFI.Function (call0, call1, call2, call3, call4)
import Went.FFI.Override (Override(..), override0, override1, override2)
import Went.Geometry.Geometry (Geometry(..))
import Went.Geometry.Margin (Margin(..))
import Went.Geometry.PathFigure (PathFigure, unPathFigure)
import Went.Geometry.PathSegment (PathSegment(..))
import Went.Geometry.Point (Point(..))
import Went.Geometry.Rect (Rect(..))
import Went.Geometry.Size (Size(..))
import Went.Geometry.Spot (Spot(..))
import Went.GraphObject.Shape.Arrowhead (Arrowhead)
import Went.GraphObject.Shape.Figure (Figure)
import Went.GraphObject.Shape.StrokeCap (StrokeCap)
import Went.GraphObject.Shape.StrokeJoin (StrokeJoin)
import Went.GraphObject.TextBlock.TextAlign (TextAlign)
import Went.Layout.LayeredDigraphLayout (AlignOption, PackOption)

class FFIMap a b | a -> b where -- TODO: why is this b -> a necessary? I think it might break things in the future
  -- UPDATE: IT DID BREAK THINGS IN THE FUTURE!
  -- bindingOfObject needs two properties, and functions *between the FFI'd types*. So there was a function
  -- that converted a string to a number, but the FFIMap class was trying to imply that the type of `opacity`
  -- in the pre-FFI record should be Int, because the FFI'd type was Number.
  ffi :: a -> b

instance maybeToNullable :: FFIMap a b => FFIMap (Maybe a) (Nullable b) where
  ffi = toNullable <<< map ffi

else instance arrayToArray :: FFIMap a b => FFIMap (Array a) (Array b) where
  ffi = map ffi

else instance intToNumber :: FFIMap Int Number where
  ffi = toNumber

else instance FFIMap Margin Margin_ where
  ffi = case _ of
    MarginEach { top, right, bottom, left } -> newMargin top right bottom left
    MarginAll x -> newMargin x x x x

else instance FFIMap Point Point_ where
  ffi (Point { x, y }) = newPoint x y

else instance FFIMap Rect Rect_ where
  ffi = case _ of
    RectEach { x, y, w, h } -> newRect x y w h
    RectAll x -> newRect x x x x

else instance FFIMap Spot Spot_ where
  ffi = case _ of
    Spot { x, y, offsetx, offsety } -> newSpot x y offsetx offsety
    TopSide -> topSide_
    TopBottomSides -> topBottomSides_
    TopLeftSides -> topLeftSides_
    TopRightSides -> topRightSides_
    BottomSide -> bottomSide_
    BottomLeftSides -> bottomLeftSides_
    BottomRightSides -> bottomRightSides_
    LeftRightSides -> leftRightSides_
    LeftSide -> leftSide_
    RightSide -> rightSide_

else instance FFIMap Size Size_ where
  ffi = case _ of
    SizeEach { w, h } -> newSize w h
    SizeBoth x -> newSize x x

else instance FFIMap PathFigure PathFigure_ where
  ffi pathFig = pathFigure_ sx sy filled shadowed isEvenOdd (ffi <$> segments)
    where
    { sx, sy, filled, shadowed, isEvenOdd, segments } = unPathFigure pathFig

else instance FFIMap PathSegment PathSegment_ where
  ffi = case _ of
    Line ex ey -> pathSegmentLine_ ex ey
    QuadraticBezier ex ey x1 y1 -> pathSegmentQuadraticBezier_ ex ey x1 y1
    Bezier ex ey x1 y1 x2 y2 -> pathSegmentBezier_ ex ey x1 y1 x2 y2
    Arc startAngle sweepAngle centerX centerY radiusX radiusY -> pathSegmentArc_ startAngle sweepAngle centerX centerY radiusX radiusY
    SvgArc ex ey radiusX radiusY xAxisRotation largeArcFlag clockwiseFlag -> pathSegmentSvgArc_ ex ey radiusX radiusY xAxisRotation largeArcFlag clockwiseFlag
    Move ex ey -> pathSegmentMove_ ex ey
    Close pathSeg -> close_ $ ffi pathSeg

else instance FFIMap Geometry Geometry_ where
  ffi (Geometry { figures, spot1, spot2 }) = newGeometry (ffi <$> figures) (ffi spot1) (ffi spot2)

-- Curried effectful functions become JavaScript uncurried functions; TODO: Turn into mkEffect calls
else instance FFIMap (a -> b -> c -> d -> Effect e) (Fn4 a b c d e) where
  ffi = call4

else instance FFIMap (a -> b -> c -> Effect d) (Fn3 a b c d) where
  ffi = call3

else instance FFIMap (a -> b -> Effect c) (Fn2 a b c) where
  ffi = call2

else instance FFIMap (a -> Effect c) (Fn1 a c) where
  ffi = call1

else instance FFIMap (Effect c) (Fn0 c) where
  ffi = call0

-- Curried pure functions become JavaScript uncurried functions
else instance (FFIMap d e) => FFIMap (a -> b -> c -> d) (Fn3 a b c e) where
  ffi f = mkFn3 $ \a b c -> ffi $ f a b c

else instance (FFIMap c d) => FFIMap (a -> b -> c) (Fn2 a b d) where
  ffi f = mkFn2 $ \a b -> ffi $ f a b

else instance (FFIMap b c) => FFIMap (a -> b) (Fn1 a c) where
  ffi f = mkFn1 $ \a -> ffi $ f a

-- Overridden methods become functions taking one less argument (at call sites, the first argument corresponds to `this`)
else instance FFIMap (Override a (b -> c -> Effect d)) (Fn2 b c d) where
  ffi (Override f) = override2 f

else instance FFIMap (Override a (b -> Effect c)) (Fn1 b c) where
  ffi (Override f) = override1 f

else instance FFIMap (Override a (Effect c)) (Fn0 c) where
  ffi (Override f) = override0 f

-- These identities need to be matched on before EnumValueFFI
else instance FFIMap String String where
  ffi = identity
else instance FFIMap Boolean Boolean where
  ffi = identity
else instance FFIMap Number Number where
  ffi = identity
else instance FFIMap (Fn4 a b c d e) (Fn4 a b c d e) where
  ffi = identity
else instance FFIMap (Fn3 a b c d) (Fn3 a b c d) where
  ffi = identity
else instance FFIMap (Fn2 a b c) (Fn2 a b c) where
  ffi = identity
else instance FFIMap (Fn1 a b) (Fn1 a b) where
  ffi = identity
else instance FFIMap (Fn0 a) (Fn0 a) where
  ffi = identity

-- Newtypes over numbers need to be matched individually
else instance FFIMap PackOption Number where
  ffi = unwrap
else instance FFIMap AlignOption Number where
  ffi = unwrap
-- Sum types that are not enumvalues
else instance FFIMap Figure String where
  ffi = show
else instance FFIMap StrokeCap String where
  ffi = show
else instance FFIMap StrokeJoin String where
  ffi = show
else instance FFIMap Arrowhead String where
  ffi = show
else instance FFIMap TextAlign String where
  ffi = show

-- It's as if Keys can be mapped to "anything"
else instance FFIMap Key a where
  ffi k = case k of
    StringKey s -> unsafeCoerce s
    NumberKey n -> unsafeCoerce n
    UndefinedKey -> unsafeCoerce k -- we need a js undefined
    
else instance FFIMap k a => FFIMap (KeyProperty nodeData k) a where
  ffi = case _ of
    Property s -> unsafeCoerce s
    FunctionProperty f -> unsafeCoerce $ mkFn2 $ \a b -> ffi $ f a b -- create a new fn that ffis the output of f, package that into Fn2, then coerce it

-- Record FFI recurses, since hmap is implemented with ffi (see below)
else instance HMap CreateFFIRecord (Record r1) (Record r2) => FFIMap (Record r1) (Record r2) where
  ffi = hmap CreateFFIRecord
else instance sumTypeToEnumValue :: EnumValueFFI a => FFIMap a EnumValue_ where
  ffi = enumValue

else instance rest :: FFIMap a a where
  ffi = identity

data CreateFFIRecord = CreateFFIRecord

instance createFFIRecord :: FFIMap a b => Mapping CreateFFIRecord a b where
  mapping CreateFFIRecord = ffi
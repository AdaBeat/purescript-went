module Went.Geometry.Geometry where

import Went.Geometry.PathFigure (PathFigure)
import Went.Geometry.Spot (Spot)

newtype Geometry = Geometry { figures :: Array PathFigure, spot1 :: Spot, spot2 :: Spot }

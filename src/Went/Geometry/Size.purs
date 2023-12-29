module Went.Geometry.Size where

data Size
  = SizeEach { w :: Number, h :: Number }
  | SizeBoth Number

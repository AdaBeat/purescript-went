module Went.Typelevel.LowercaseFirst where

import Data.Symbol (class IsSymbol)
import Prim.Symbol (class Cons)

class LowercaseChar (cUpper :: Symbol) (c :: Symbol) | c -> cUpper, cUpper -> c

instance LowercaseChar "A" "a"
instance LowercaseChar "B" "b"
instance LowercaseChar "C" "c"
instance LowercaseChar "D" "d"
instance LowercaseChar "E" "e"
instance LowercaseChar "F" "f"
instance LowercaseChar "G" "g"
instance LowercaseChar "H" "h"
instance LowercaseChar "I" "i"
instance LowercaseChar "J" "j"
instance LowercaseChar "K" "k"
instance LowercaseChar "L" "l"
instance LowercaseChar "M" "m"
instance LowercaseChar "N" "n"
instance LowercaseChar "O" "o"
instance LowercaseChar "P" "p"
instance LowercaseChar "Q" "q"
instance LowercaseChar "R" "r"
instance LowercaseChar "S" "s"
instance LowercaseChar "T" "t"
instance LowercaseChar "U" "u"
instance LowercaseChar "V" "v"
instance LowercaseChar "W" "w"
instance LowercaseChar "X" "x"
instance LowercaseChar "Y" "y"
instance LowercaseChar "Z" "z"

class IsSymbol out <= LowercaseFirstAux (h :: Symbol) (t :: Symbol) (out :: Symbol) | h t -> out

instance
  ( LowercaseChar hUpper h
  , IsSymbol out
  , Cons h t out
  ) =>
  LowercaseFirstAux hUpper t out

class IsSymbol out <= LowercaseFirst (inp :: Symbol) (out :: Symbol) | inp -> out

instance
  ( Cons h t inp
  , LowercaseFirstAux h t out
  ) =>
  LowercaseFirst inp out

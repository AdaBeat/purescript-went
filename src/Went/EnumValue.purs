module Went.EnumValue where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import GoJS.EnumValue (EnumValue_, enumValueBuilder_)
import Type.Proxy (Proxy(..))

class (Show a, IsSymbol className) <= EnumValue (className :: Symbol) (a :: Type) | a -> className

-- Defining sum types as instances as EnumValue, with the right symbol for their className,
-- is now enough for them to be mapped to the right static value of EnumValue in Went.
instance (EnumValue className a) => EnumValueFFI a where
  enumValue enum = enumValueBuilder_ (reflectSymbol (Proxy @className)) (show enum)

-- This class is used by PureScript sum types to convert them to one of the below EnumValue_.
-- This makes it so that sum types in the purescript world can remain defined in the module where they're relevant.
class EnumValueFFI a where
  enumValue :: a -> EnumValue_

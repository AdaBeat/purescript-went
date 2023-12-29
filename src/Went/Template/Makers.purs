module Went.Template.Makers where

import Effect (Effect)

-- These types exist solely for the purpose of only being creatable via their associated constructors,
-- which are defined in the modules where they are relevant. In some cases, they exist in order to contain
-- from their creators model information.
--
-- These constructors will usually accept newtypes over readers, which the functions constructing
-- values of these types will then effectfully create the arguments for and then pass to.
newtype Made (bindable :: Row Type) a = Made (Effect a)

type MadeNode (nodeData :: Row Type) a = Made nodeData a
type MadeLink (linkData :: Row Type) a = Made linkData a
type MadeGroup (nodeData :: Row Type) a = Made nodeData a


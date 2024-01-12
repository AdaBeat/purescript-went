module Went.GraphObject.Panel.Part.Adornment where

import GoJS.GraphObject.Types (Adornment_)
import Went.GraphObject (GraphObjectSpecificFields)
import Went.GraphObject.Panel (PanelSpecificFields)
import Went.GraphObject.Panel.Part (PartSpecificFields)

type AdornmentSpecificFields (a :: Row Type) =
  ( 
    -- Adornments have no specific fields that make sense to set statically.
    | a
  )

type AdornmentFields (extraFields :: Row Type) =
  GraphObjectSpecificFields Adornment_
    ( PanelSpecificFields
        ( PartSpecificFields Adornment_
            ( AdornmentSpecificFields extraFields
            )
        )
    )

module FieldNameAliases exposing (..)

{-| //UseFieldNameMapping(couchConversion)
-}


type alias R =
    { id : String
    , rev : String
    , payload : List Int
    }


{-| //FieldNameMapping
-}
couchConversion =
    { id = "_id"
    , rev = "_rev"
    }

module FieldNameAliases exposing (..)

{-| //UseFieldNameConversion(couchConversion)
-}


type alias R =
    { id : String
    , rev : String
    , payload : List Int
    }


{-| //NameConversion
-}
couchConversion =
    { id = "_id"
    , rev = "_rev"
    }

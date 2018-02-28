module WithDefaultValues exposing (..)


type A
    = Trivial
    | B Int
    | C String


type alias R =
    { a : Int
    , b : List String
    }


{-| //DefaultValue
-}
defaultA : A
defaultA =
    B 3


{-| //DefaultValue
-}
initR : R -> R
initR r =
    { r | b = [ "a" ] }

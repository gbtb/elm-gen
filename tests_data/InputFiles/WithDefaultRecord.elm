module WithDefaultRecord exposing (..)


type alias A =
    { r : R
    }


type alias R =
    { i : Int
    }


{-| //DefaultValue
-}
initA : A
initA =
    { r = { i = 1 }
    }

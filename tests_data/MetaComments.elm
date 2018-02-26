module MetaComments exposing (..)

-- //Ignore


type A
    = Trivial
    | B Int
    | C String


{-| //Ignore
-}
type alias R =
    { a : Int
    , b : List Char
    }


type C
    = Cons1 Int
    | Cons2 String Int Float

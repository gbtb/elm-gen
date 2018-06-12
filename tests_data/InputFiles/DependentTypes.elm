module DependentTypes exposing (..)


type Basic
    = Trivial
    | Cons1 Int
    | Cons2 (List String)


type alias Record =
    { field1 : List Float
    , field2 : Basic
    }

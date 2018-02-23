module Config exposing (..)


type ProvidedNameModification
    = DontTouch
    | Replace String


type alias NameModification =
    { prefix : String
    , suffix : String
    , providedName : ProvidedNameModification
    }


type UnionTypeGeneratorFeature
    = TrivialString
    | TrivialObject
    | DefaultConstructor

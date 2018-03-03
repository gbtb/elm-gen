module Config exposing (..)


type ProvidedNameModification
    = DontTouch
    | Replace String


type alias NameModification =
    { prefix : String
    , suffix : String
    , providedName : ProvidedNameModification
    }


{-| //DefaultValue
-}
initNameMod : NameModification
initNameMod =
    { prefix = ""
    , suffix = ""
    , providedName = DontTouch
    }


type UnionTypeGeneratorFeature
    = TrivialString
    | TrivialObject
    | DefaultConstructor

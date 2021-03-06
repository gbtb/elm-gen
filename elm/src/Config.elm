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


type alias JsonModulesImports =
    { decode : ProvidedNameModification
    , encode : ProvidedNameModification
    }


getDecodePrefix conf =
    case conf of
        DontTouch ->
            "Json.Decode"

        Replace str ->
            str


getEncodePrefix conf =
    case conf of
        DontTouch ->
            "Json.Encode"

        Replace str ->
            str

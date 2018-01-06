module ThreeLevelsDepth exposing (..)

import DependentOnOtherFile exposing (DependentType)


type alias NewType =
    { foo : List (List String)
    , bar : Maybe DependentType
    }

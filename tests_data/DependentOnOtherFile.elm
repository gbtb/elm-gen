module DependentOnOtherType exposing (..)

import DependentTypes exposing (Basic(..), Record(..))


type DependentType
    = A
    | B Basic
    | C Record

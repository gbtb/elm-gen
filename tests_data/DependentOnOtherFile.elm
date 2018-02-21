module DependentOnOtherFile exposing (..)

import DependentTypes exposing (Basic(..), Record)


type DependentType
    = A
    | B Basic
    | C Record

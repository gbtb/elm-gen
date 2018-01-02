module Utils exposing (..)


fromJust err m =
    case m of
        Just x ->
            x

        Nothing ->
            Debug.crash err

module HttpErrors exposing (..)

import Http
import Model exposing (..)


setErrors : Model -> Http.Error -> ( Model, Cmd Msg )
setErrors model error =
    { model | httpError = Just error } ! []

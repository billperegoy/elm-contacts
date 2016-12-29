module TagActions exposing (..)

import Model exposing (..)
import Http
import Tag exposing (..)
import HttpErrors


receive : Model -> TagsResponse -> ( Model, Cmd Msg )
receive model response =
    { model
        | tags = response.tags
        , httpError = Nothing
    }
        ! []


request : Cmd Msg
request =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/tags"
    in
        Http.send ProcessTags (Http.get url tagsResponseDecoder)

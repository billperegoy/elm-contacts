module TagActions exposing (..)

import Model exposing (..)
import Http
import Tag exposing (..)
import HttpErrors


processTags : Model -> TagsResponse -> ( Model, Cmd Msg )
processTags model response =
    { model
        | tags = response.tags
        , httpError = Nothing
    }
        ! []


getTags : Cmd Msg
getTags =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/tags"
    in
        Http.send ProcessTags (Http.get url tagsResponseDecoder)

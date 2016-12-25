module HttpUtils exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Html exposing (..)
import Html.Events exposing (..)


deleteResponseDecoder : Json.Decode.Decoder DeleteResponse
deleteResponseDecoder =
    Json.Decode.Pipeline.decode DeleteResponse
        |> Json.Decode.Pipeline.required "activity_id" Json.Decode.string


type alias DeleteResponse =
    { activityId : String
    }


onClickNoDefault : msg -> Attribute msg
onClickNoDefault message =
    let
        config =
            { stopPropagation = True
            , preventDefault = True
            }
    in
        onWithOptions "click" config (Json.Decode.succeed message)

module HttpUtils exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Html exposing (..)
import Html.Events exposing (..)
import Http


type HttpAction
    = Get
    | Put
    | Post
    | Delete


deleteResponseDecoder : Json.Decode.Decoder MassActionResponse
deleteResponseDecoder =
    Json.Decode.Pipeline.decode MassActionResponse
        |> Json.Decode.Pipeline.required "activity_id" Json.Decode.string


type alias MassActionResponse =
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


type alias V3ApiError =
    { errorKey : String
    , errorMessage : String
    }


v3ApiErrorDecoder : Json.Decode.Decoder V3ApiError
v3ApiErrorDecoder =
    Json.Decode.Pipeline.decode V3ApiError
        |> Json.Decode.Pipeline.required "error_key" Json.Decode.string
        |> Json.Decode.Pipeline.required "error_message" Json.Decode.string


v3ApiErrorListDecoder : Json.Decode.Decoder (List V3ApiError)
v3ApiErrorListDecoder =
    Json.Decode.list v3ApiErrorDecoder


httpActionToString : HttpAction -> String
httpActionToString action =
    case action of
        Get ->
            "GET"

        Put ->
            "PUT"

        Post ->
            "POST"

        Delete ->
            "DELETE"


errorString : Http.Error -> String
errorString error =
    case error of
        Http.BadUrl url ->
            toString error

        Http.Timeout ->
            toString error

        Http.NetworkError ->
            toString error

        Http.BadStatus response ->
            let
                decodedResponse =
                    Json.Decode.decodeString v3ApiErrorListDecoder response.body
            in
                case decodedResponse of
                    Ok result ->
                        List.map .errorMessage result |> String.join " "

                    Err err ->
                        toString err

        Http.BadPayload payload response ->
            toString error

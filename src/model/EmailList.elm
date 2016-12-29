module EmailList exposing (..)

import Json.Decode
import Json.Decode.Pipeline


type alias EmailListResponse =
    { lists : List EmailList
    }


type alias EmailList =
    { id : String
    , name : String
    , favorite : Bool
    }


emailListResponseDecoder : Json.Decode.Decoder EmailListResponse
emailListResponseDecoder =
    Json.Decode.Pipeline.decode EmailListResponse
        |> Json.Decode.Pipeline.required "lists" emailListListDecoder


emailListListDecoder : Json.Decode.Decoder (List EmailList)
emailListListDecoder =
    Json.Decode.list emailListDecoder


emailListDecoder : Json.Decode.Decoder EmailList
emailListDecoder =
    Json.Decode.Pipeline.decode EmailList
        |> Json.Decode.Pipeline.required "list_id" Json.Decode.string
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "favorite" Json.Decode.bool

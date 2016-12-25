module Tag exposing (..)

import Json.Decode
import Json.Decode.Pipeline


--
-- Tags
--


type alias TagsResponse =
    { tags : List Tag
    }


type alias Tag =
    { id : String
    , name : String
    }


tagsResponseDecoder : Json.Decode.Decoder TagsResponse
tagsResponseDecoder =
    Json.Decode.Pipeline.decode TagsResponse
        |> Json.Decode.Pipeline.required "tags" tagListDecoder


tagListDecoder : Json.Decode.Decoder (List Tag)
tagListDecoder =
    Json.Decode.list tagDecoder


tagDecoder : Json.Decode.Decoder Tag
tagDecoder =
    Json.Decode.Pipeline.decode Tag
        |> Json.Decode.Pipeline.required "tag_id" Json.Decode.string
        |> Json.Decode.Pipeline.required "name" Json.Decode.string

module HttpUtils exposing (..)

import Json.Decode
import Json.Decode.Pipeline


deleteResponseDecoder : Json.Decode.Decoder DeleteResponse
deleteResponseDecoder =
    Json.Decode.Pipeline.decode DeleteResponse
        |> Json.Decode.Pipeline.required "activity_id" Json.Decode.string


type alias DeleteResponse =
    { activityId : String
    }

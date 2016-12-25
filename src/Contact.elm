module Contact exposing (..)

import Json.Decode
import Json.Decode.Pipeline


--
-- Contacts
--


type alias ContactsResponse =
    { count : Int
    , contacts : List Contact
    , links : Links
    }


type alias Contact =
    { firstName : Maybe String
    , lastName : Maybe String
    , email : Email
    }


type alias Email =
    { address : String
    }


type ContactsFilterState
    = All
    | Unsubscribed
    | ByTag String
    | ByList String


type PaginationDirection
    = Forward
    | Backward


type alias Links =
    { next : Link
    , previous : Link
    }


type alias Link =
    { url : String
    }


contactResponseDecoder : Json.Decode.Decoder ContactsResponse
contactResponseDecoder =
    Json.Decode.Pipeline.decode ContactsResponse
        |> Json.Decode.Pipeline.required "contacts_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "contacts" contactListDecoder
        |> Json.Decode.Pipeline.optional "_links"
            linksDecoder
            { next = { url = "" }, previous = { url = "" } }


linksDecoder : Json.Decode.Decoder Links
linksDecoder =
    Json.Decode.Pipeline.decode Links
        |> Json.Decode.Pipeline.optional "next"
            linkDecoder
            { url = "" }
        |> Json.Decode.Pipeline.optional "previous"
            linkDecoder
            { url = "" }


linkDecoder : Json.Decode.Decoder Link
linkDecoder =
    Json.Decode.Pipeline.decode Link
        |> Json.Decode.Pipeline.required "href" Json.Decode.string


contactListDecoder : Json.Decode.Decoder (List Contact)
contactListDecoder =
    Json.Decode.list contactDecoder


contactDecoder : Json.Decode.Decoder Contact
contactDecoder =
    Json.Decode.Pipeline.decode Contact
        |> Json.Decode.Pipeline.required "first_name" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "last_name" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "email_address" emailDecoder


emailDecoder : Json.Decode.Decoder Email
emailDecoder =
    Json.Decode.Pipeline.decode Email
        |> Json.Decode.Pipeline.required "address" Json.Decode.string

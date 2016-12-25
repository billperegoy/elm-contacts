module Model exposing (..)

import Http
import Json.Decode
import Json.Decode.Pipeline


type alias Model =
    { contactsCount : Int
    , displayContactsPerPageMenu : Bool
    , contactsPerPage : Int
    , contacts : List Contact
    , startContactIndex : Int
    , nextContactsUrl : Maybe String
    , previousContactsUrl : Maybe String
    , filterState : ContactsFilterState
    , tags : List Tag
    , lists : List EmailList
    , httpError : String
    , showRenameModal : Bool
    , activeList : Maybe EmailList
    , newListName : String
    }


type Msg
    = ProcessContacts (Result Http.Error ContactsResponse)
    | ProcessEmailLists (Result Http.Error EmailListResponse)
    | ProcessTags (Result Http.Error TagsResponse)
    | GetContacts ContactsFilterState
    | GetPaginatedContacts PaginationDirection String
    | DisplaySetContactsPerPageMenu
    | SetContactsPerPage Int
    | ShowRenameListModal EmailList
    | UpdateNewListName String
    | DeleteList String
    | CompleteListRename
    | AcknowledgeDialog
    | ProcessListPut (Result Http.Error EmailList)
    | ProcessListDelete (Result Http.Error DeleteResponse)


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


type alias EmailListResponse =
    { lists : List EmailList
    }


type alias EmailList =
    { id : String
    , name : String
    }


type alias TagsResponse =
    { tags : List Tag
    }


type alias Tag =
    { id : String
    , name : String
    }


type ContactsFilterState
    = All
    | Unsubscribed
    | ByTag String
    | ByList String


type PaginationDirection
    = Forward
    | Backward


type alias DeleteResponse =
    { activityId : String
    }


type alias Links =
    { next : Link
    , previous : Link
    }


type alias Link =
    { url : String
    }


init : ( Model, Cmd Msg )
init =
    let
        contactsPerPage =
            50
    in
        { contactsCount = 0
        , contactsPerPage = contactsPerPage
        , displayContactsPerPageMenu = False
        , contacts = []
        , startContactIndex = 1
        , nextContactsUrl = Nothing
        , previousContactsUrl = Nothing
        , filterState = All
        , tags = []
        , lists = []
        , httpError = ""
        , showRenameModal = False
        , activeList = Nothing
        , newListName = ""
        }
            ! [ getContacts All contactsPerPage, getEmailLists, getTags ]


getTags : Cmd Msg
getTags =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/tags"
    in
        Http.send ProcessTags (Http.get url tagsResponseDecoder)


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


emailDecoder : Json.Decode.Decoder Email
emailDecoder =
    Json.Decode.Pipeline.decode Email
        |> Json.Decode.Pipeline.required "address" Json.Decode.string


httpParams : List ( String, String ) -> String
httpParams params =
    let
        paramStringList =
            List.map (\( name, value ) -> name ++ "=" ++ value) params
    in
        String.join "&" paramStringList


urlString : String -> List ( String, String ) -> String
urlString baseUrl params =
    baseUrl
        ++ "?"
        ++ httpParams params


getContacts : ContactsFilterState -> Int -> Cmd Msg
getContacts filter contactsPerPage =
    let
        baseUrl =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/contacts"

        sortParam =
            ( "sort", "contacts.last_name" )

        limitParam =
            ( "limit", toString contactsPerPage )

        countParam =
            ( "include_count", "true" )

        commonParams =
            [ sortParam, countParam, limitParam ]

        url =
            case filter of
                All ->
                    urlString baseUrl commonParams

                Unsubscribed ->
                    urlString baseUrl (( "status", "unsubscribed" ) :: commonParams)

                ByTag id ->
                    urlString baseUrl (( "tags", id ) :: commonParams)

                ByList id ->
                    urlString baseUrl (( "lists", id ) :: commonParams)
    in
        Http.send ProcessContacts (Http.get url contactResponseDecoder)


getPaginatedContacts : String -> Cmd Msg
getPaginatedContacts path =
    let
        url =
            "http://0.0.0.0:3000/" ++ path
    in
        Http.send ProcessContacts (Http.get url contactResponseDecoder)


getEmailLists : Cmd Msg
getEmailLists =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists"
    in
        Http.send ProcessEmailLists (Http.get url emailListResponseDecoder)


putList : String -> String -> Cmd Msg
putList id newName =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists/" ++ id

        body =
            Http.stringBody "application/json"
                ("""{"name" : """ ++ "\"" ++ newName ++ "\"" ++ """, "favorite" : "false"}""")

        request =
            Http.request
                { method = "PUT"
                , headers = []
                , url = url
                , body = body
                , expect = Http.expectJson emailListDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send ProcessListPut request


deleteResponseDecoder : Json.Decode.Decoder DeleteResponse
deleteResponseDecoder =
    Json.Decode.Pipeline.decode DeleteResponse
        |> Json.Decode.Pipeline.required "activity_id" Json.Decode.string


deleteList : String -> Cmd Msg
deleteList id =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists/" ++ id

        request =
            Http.request
                { method = "DELETE"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson deleteResponseDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send ProcessListDelete request


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

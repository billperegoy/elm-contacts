module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Json.Decode.Pipeline
import ApiKeys


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { contactsCount : Int
    , contactsPerPage : Int
    , contacts : List Contact
    , filterState : ContactsFilterState
    , tags : List Tag
    , lists : List EmailList
    , error : String
    }


type alias ContactsResponse =
    { count : Int
    , contacts : List Contact
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


init : ( Model, Cmd Msg )
init =
    { contactsCount = 0
    , contactsPerPage = 50
    , contacts = []
    , filterState = All
    , tags = []
    , lists = []
    , error = ""
    }
        ! [ getContacts All 50, getEmailLists, getTags ]


type ContactsFilterState
    = All
    | Unsubscribed
    | ByTag String
    | ByList String



-- Update


type Msg
    = ProcessContacts (Result Http.Error ContactsResponse)
    | ProcessEmailLists (Result Http.Error EmailListResponse)
    | ProcessTags (Result Http.Error TagsResponse)
    | FilterByList String
    | FilterByTag String
    | GetAllContacts
    | GetUnsubscribedContacts
      --
    | UpdateSearchString String
    | Search
    | ShowAdvancedSearchModal
      --
    | ShowAddListModal
    | AddNewList String
    | ShowListMenu
    | ShowRenameListModal
    | ShowDeleteListModal
    | DeleteList String
    | DeleteListAndContacts
    | UpdateListName String String
      --
    | ShowAddTagModal
    | AddNewTag String
    | ShowTagMenu
    | ShowRanmeTagModal
    | ShowDeleteTagModal
    | DeleteTag String
    | DeleteTagAndContacts
    | UpdateTagName String String
      --
    | SelectContact
    | SelectAllContacts
    | AddContactToLists
    | RemoveContactFromLists
      --
    | ShowManageTagsModal
    | SelectTag
    | SearchForTag
    | CreateNewTag
    | SaveContactTagChanges
      --
    | ShowQuickSendModal
    | DeleteContact
      --
    | AddSingleContact
    | AddMultipleContacts
    | UploadContacts
    | ImportContacts


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProcessContacts (Ok response) ->
            { model
                | contacts = response.contacts
                , contactsCount = response.count
            }
                ! []

        ProcessEmailLists (Ok response) ->
            { model
                | lists = response.lists
            }
                ! []

        ProcessTags (Ok response) ->
            { model
                | tags = response.tags
            }
                ! []

        GetAllContacts ->
            { model
                | contacts = []
                , contactsCount = 0
                , filterState = All
            }
                ! [ getContacts All model.contactsPerPage ]

        GetUnsubscribedContacts ->
            { model
                | contacts = []
                , contactsCount = 0
                , filterState = Unsubscribed
            }
                ! [ getContacts Unsubscribed model.contactsPerPage ]

        FilterByList listId ->
            { model
                | contacts = []
                , contactsCount = 0
                , filterState = ByList listId
            }
                ! [ getContacts (ByList listId) model.contactsPerPage ]

        FilterByTag tagId ->
            { model
                | contacts = []
                , contactsCount = 0
                , filterState = ByTag tagId
            }
                ! [ getContacts (ByTag tagId) model.contactsPerPage ]

        ProcessContacts (Err error) ->
            { model | error = toString error } ! []

        ProcessEmailLists (Err error) ->
            { model | error = toString error } ! []

        ProcessTags (Err error) ->
            { model | error = toString error } ! []

        _ ->
            model ! []



-- View


tableHeader : Html Msg
tableHeader =
    thead []
        [ tr []
            [ th [] [ text "Last Name" ]
            , th [] [ text "First Name" ]
            , th [] [ text "Email" ]
            ]
        ]


tableBody : List Contact -> Html Msg
tableBody contacts =
    tbody []
        (contactRows contacts)


contactsTable : List Contact -> Html Msg
contactsTable contacts =
    table [ class "table table-striped" ]
        [ tableHeader
        , tableBody contacts
        ]


contactRows : List Contact -> List (Html Msg)
contactRows contacts =
    List.map
        (\contact ->
            tr []
                [ td [] [ text (Maybe.withDefault "" contact.lastName) ]
                , td [] [ text (Maybe.withDefault "" contact.firstName) ]
                , td [] [ text contact.email.address ]
                ]
        )
        contacts


tagName : String -> List Tag -> String
tagName id tags =
    tags
        |> List.filter (\tag -> tag.id == id)
        |> List.head
        |> Maybe.withDefault { name = "unknown", id = "inknown" }
        |> .name


listName : String -> List EmailList -> String
listName id lists =
    lists
        |> List.filter (\list -> list.id == id)
        |> List.head
        |> Maybe.withDefault { name = "unknown", id = "inknown" }
        |> .name


contactsCount : Model -> Html Msg
contactsCount model =
    let
        displayText =
            case model.filterState of
                All ->
                    "All Contacts (" ++ toString model.contactsCount ++ ")"

                Unsubscribed ->
                    "Unsubscribed (" ++ toString model.contactsCount ++ ")"

                ByTag id ->
                    tagName id model.tags ++ " (" ++ toString model.contactsCount ++ ")"

                ByList id ->
                    listName id model.lists ++ " (" ++ toString model.contactsCount ++ ")"
    in
        h2 []
            [ span [ class "label label-primary" ]
                [ text displayText ]
            ]


errors : String -> Html Msg
errors errorString =
    if errorString == "" then
        div [] []
    else
        div [ class "alert alert-danger" ] [ text errorString ]


sidebar : List EmailList -> List Tag -> Html Msg
sidebar lists tags =
    div [ class "col-md-3" ]
        [ h4 []
            [ span [ class "label label-success" ] [ text "contacts" ]
            ]
        , ul []
            [ li [] [ text "active" ]
            , li [] [ a [ onClick GetUnsubscribedContacts, href "#" ] [ text "unsubscribed" ] ]
            , li [] [ a [ onClick GetAllContacts, href "#" ] [ text "view all contacts" ] ]
            ]
        , h4 []
            [ span [ class "label label-success" ] [ text "email lists" ]
            ]
        , ul []
            (List.map
                (\list ->
                    li []
                        [ a [ onClick (FilterByList list.id), href "#" ] [ text list.name ]
                        ]
                )
                lists
            )
        , h4 []
            [ span [ class "label label-success" ] [ text "tags" ]
            ]
        , ul []
            (List.map
                (\tag ->
                    li []
                        [ a [ onClick (FilterByTag tag.id), href "#" ] [ text tag.name ]
                        ]
                )
                tags
            )
        ]


mainContent : Model -> Html Msg
mainContent model =
    div [ class "col-md-9" ]
        [ (errors model.error)
        , (contactsCount model)
        , (contactsTable model.contacts)
        ]


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ div
            [ class "row" ]
            [ (sidebar model.lists model.tags)
            , (mainContent model)
            ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Http


headers : List Http.Header
headers =
    [ Http.header "x-api-key" ApiKeys.apiKey
    , Http.header "authorization" ApiKeys.authorization
    , Http.header "accept" "application/json"
    , Http.header "content_type" "application/json"
    ]


getContacts : ContactsFilterState -> Int -> Cmd Msg
getContacts filter contactsPerPage =
    let
        url =
            case filter of
                All ->
                    "http://0.0.0.0:3000/contacts-service/v3/accounts/1/contacts?sort=contacts.last_name&include_count=true&limit=500"

                Unsubscribed ->
                    "http://0.0.0.0:3000/contacts-service/v3/accounts/1/contacts?sort=contacts.last_name&include_count=true&limit=500&status=unsubscribed"

                ByTag id ->
                    "http://0.0.0.0:3000/contacts-service/v3/accounts/1/contacts?sort=contacts.last_name&include_count=true&limit=500&tags=" ++ id

                ByList id ->
                    "http://0.0.0.0:3000/contacts-service/v3/accounts/1/contacts?sort=contacts.last_name&include_count=true&limit=500&lists=" ++ id
    in
        Http.send ProcessContacts
            (Http.request
                { method = "GET"
                , headers = headers
                , url = Debug.log "URL: " url
                , body = Http.emptyBody
                , expect = Http.expectJson contactResponseDecoder
                , timeout = Nothing
                , withCredentials = False
                }
            )


getEmailLists : Cmd Msg
getEmailLists =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists"
    in
        Http.send ProcessEmailLists
            (Http.request
                { method = "GET"
                , headers = headers
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson emailListResponseDecoder
                , timeout = Nothing
                , withCredentials = False
                }
            )


getTags : Cmd Msg
getTags =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/tags"
    in
        Http.send ProcessTags
            (Http.request
                { method = "GET"
                , headers = headers
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson tagsResponseDecoder
                , timeout = Nothing
                , withCredentials = False
                }
            )


contactResponseDecoder : Json.Decode.Decoder ContactsResponse
contactResponseDecoder =
    Json.Decode.Pipeline.decode ContactsResponse
        |> Json.Decode.Pipeline.required "contacts_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "contacts" contactListDecoder


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

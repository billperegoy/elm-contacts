module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
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
    { contacts : List Contact
    , tags : List Tag
    , lists : List List
    , error : String
    }


type alias ContactsResponse =
    { contacts : List Contact
    }


type alias Contact =
    { firstName : String
    , lastName : String
    , email : Email
    }


type alias Email =
    { address : String
    }


type ContactState
    = NotSet
    | TempHold
    | PendingConfirmation
    | Implicit
    | Explicit
    | OptOut
    | Deprecated
    | Active


type alias EmailList =
    { name : String
    }


type alias Phone =
    { kind : PhoneType
    , number : String
    }


type PhoneType
    = Home
    | Work
    | Mobile
    | Fax
    | Other


type CustomField
    = Website String
    | Twitter String
    | Facebook String
    | Google String
    | Flickr String
    | Custom String String


type alias Tag =
    { name : String }


init : ( Model, Cmd Msg )
init =
    { contacts = []
    , tags = []
    , lists = []
    , error = ""
    }
        ! [ getContacts ]


type ContactFilterType
    = ContactActive
    | ContactUnsubscribed



-- Update


type Msg
    = ProcessContacts (Result Http.Error ContactsResponse)
    | FilterByState ContactFilterType
    | FilterByList String
    | FilterByTag String
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
            { model | contacts = response.contacts } ! []

        ProcessContacts (Err error) ->
            { model | error = toString error } ! []

        _ ->
            model ! []



-- View


contactRows : List Contact -> List (Html Msg)
contactRows contacts =
    List.map
        (\contact ->
            tr []
                [ td [] [ text contact.lastName ]
                , td [] [ text contact.firstName ]
                , td [] [ text contact.email.address ]
                ]
        )
        contacts


contactsCount : List Contact -> Html Msg
contactsCount contacts =
    let
        count =
            contacts |> List.length
    in
        h2 []
            [ text ("Contacts: " ++ toString count)
            ]


errors : Model -> Html Msg
errors model =
    if model.error == "" then
        div [] []
    else
        div [ class "alert alert-danger" ] [ text model.error ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ (errors model)
        , (contactsCount model.contacts)
        , table [ class "table table-striped" ]
            [ tbody []
                (contactRows model.contacts)
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


getContacts : Cmd Msg
getContacts =
    let
        url =
            "https://api.cc.email/v3/contacts?limit=500&sort=contacts.last_name"
    in
        Http.send ProcessContacts
            (Http.request
                { method = "GET"
                , headers = headers
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson contactResponseDecoder
                , timeout = Nothing
                , withCredentials = False
                }
            )


contactResponseDecoder : Json.Decode.Decoder ContactsResponse
contactResponseDecoder =
    Json.Decode.Pipeline.decode ContactsResponse
        |> Json.Decode.Pipeline.required "contacts" contactListDecoder


contactListDecoder : Json.Decode.Decoder (List Contact)
contactListDecoder =
    Json.Decode.list contactDecoder


contactDecoder : Json.Decode.Decoder Contact
contactDecoder =
    Json.Decode.Pipeline.decode Contact
        |> Json.Decode.Pipeline.required "first_name" Json.Decode.string
        |> Json.Decode.Pipeline.required "last_name" Json.Decode.string
        |> Json.Decode.Pipeline.required "email_address" emailDecoder


emailDecoder : Json.Decode.Decoder Email
emailDecoder =
    Json.Decode.Pipeline.decode Email
        |> Json.Decode.Pipeline.required "address" Json.Decode.string

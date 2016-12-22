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
    { contactsCount : Int
    , contacts : List Contact
    , tags : List Tag
    , lists : List List
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
    { contactsCount = 0
    , contacts = []
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
            { model
                | contacts = response.contacts
                , contactsCount = response.count
            }
                ! []

        ProcessContacts (Err error) ->
            { model | error = toString error } ! []

        _ ->
            model ! []



-- View


tableHeader : List (Html Msg)
tableHeader =
    [ thead []
        [ tr []
            [ th [] [ text "Last Name" ]
            , th [] [ text "First Name" ]
            , th [] [ text "Email" ]
            ]
        ]
    ]


contactsTable : List Contact -> Html Msg
contactsTable contacts =
    table [ class "table table-striped" ]
        [ {- thead []
             [ tr []
                 [ th [] [ text "Last Name" ]
                 , th [] [ text "First Name" ]
                 , th [] [ text "Email" ]
                 ]
             ]
               ,
          -}
          tbody []
            (contactRows contacts)
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


contactsCount : Int -> Html Msg
contactsCount count =
    h2 []
        [ span [ class "label label-primary" ]
            [ text ("Contacts: " ++ toString count) ]
        ]


errors : String -> Html Msg
errors errorString =
    if errorString == "" then
        div [] []
    else
        div [ class "alert alert-danger" ] [ text errorString ]


sidebar : Html Msg
sidebar =
    div [ class "col-md-3" ]
        [ h2 []
            [ span
                [ class "label label-default" ]
                [ text "sidebar" ]
            ]
        ]


mainContent : Model -> Html Msg
mainContent model =
    div [ class "col-md-9" ]
        [ (errors model.error)
        , (contactsCount model.contactsCount)
        , (contactsTable model.contacts)
        ]


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ div
            [ class "row" ]
            [ sidebar
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


getContacts : Cmd Msg
getContacts =
    let
        url =
            --"https://api.cc.email/v3/contacts?limit=500&sort=contacts.last_name"
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/contacts?sort=contacts.last_name&include_count=true"
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

module ContactListActions exposing (..)

import Model exposing (..)
import Http
import HttpUtils exposing (..)
import Json.Encode


showModal : Model -> ( Model, Cmd Msg )
showModal model =
    { model
        | showAddContactsToListsModal = True
        , selectedLists = []
    }
        ! []


closeModal : Model -> ( Model, Cmd Msg )
closeModal model =
    { model
        | showAddContactsToListsModal = False
        , httpError = Nothing
    }
        ! []


contactsCheckbox : Model -> String -> Bool -> ( Model, Cmd Msg )
contactsCheckbox model id state =
    if state == True then
        { model | selectedContacts = id :: model.selectedContacts } ! []
    else
        { model
            | selectedContacts =
                List.filter (\e -> e /= id) model.selectedContacts
        }
            ! []


listCheckbox : Model -> String -> Bool -> ( Model, Cmd Msg )
listCheckbox model id state =
    if state == True then
        { model | selectedLists = id :: model.selectedLists } ! []
    else
        { model
            | selectedLists =
                List.filter (\e -> e /= id) model.selectedLists
        }
            ! []


complete : Model -> ( Model, Cmd Msg )
complete model =
    model ! []


submit : Model -> ( Model, Cmd Msg )
submit model =
    { model
        | showAddContactsToListsModal = False
    }
        ! [ postAddContactsToLists model ]


postAddContactsToLists : Model -> Cmd Msg
postAddContactsToLists model =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/activities/add_list_memberships"

        listIds =
            List.map Json.Encode.string model.selectedLists

        contactIds =
            List.map Json.Encode.string model.selectedContacts

        contactIdsObj =
            Json.Encode.object [ ( "contact_ids", Json.Encode.list contactIds ) ]

        payload =
            Json.Encode.object
                [ ( "list_ids", Json.Encode.list listIds )
                , ( "source", contactIdsObj )
                ]

        body =
            Http.stringBody "application/json" (Json.Encode.encode 0 payload)

        request =
            Http.request
                { method = httpActionToString Post
                , headers = []
                , url = url
                , body = body
                , expect = Http.expectJson deleteResponseDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send CompleteAddContactsToList request

module ContactListActions exposing (..)

import Model exposing (..)
import Http
import HttpUtils exposing (..)
import Json.Encode


showModal : Model -> ( Model, Cmd Msg )
showModal model =
    let
        lists =
            model.lists

        updatedLists =
            { lists
                | showAddContactsToListsModal = True
                , selected = []
            }
    in
        { model
            | lists = updatedLists
        }
            ! []


closeModal : Model -> ( Model, Cmd Msg )
closeModal model =
    let
        lists =
            model.lists

        updatedLists =
            { lists | showAddContactsToListsModal = False }
    in
        { model
            | lists = updatedLists
            , httpError = Nothing
        }
            ! []


contactsCheckbox : Model -> String -> Bool -> ( Model, Cmd Msg )
contactsCheckbox model id state =
    let
        contacts =
            model.contacts

        updatedContactsAdd =
            { contacts | selected = id :: model.contacts.selected }

        updatedContactsDelete =
            { contacts
                | selected =
                    List.filter (\e -> e /= id) model.contacts.selected
            }
    in
        if state == True then
            { model | contacts = updatedContactsAdd } ! []
        else
            { model
                | contacts = updatedContactsDelete
            }
                ! []


listCheckbox : Model -> String -> Bool -> ( Model, Cmd Msg )
listCheckbox model id state =
    let
        lists =
            model.lists

        updatedListsAdd =
            { lists | selected = id :: model.lists.selected }

        updatedListsDelete =
            { lists | selected = (List.filter (\e -> e /= id) model.lists.selected) }
    in
        if state == True then
            { model | lists = updatedListsAdd } ! []
        else
            { model
                | lists = updatedListsDelete
            }
                ! []


complete : Model -> ( Model, Cmd Msg )
complete model =
    model ! []


submit : Model -> ( Model, Cmd Msg )
submit model =
    let
        lists =
            model.lists

        updatedLists =
            { lists | showAddContactsToListsModal = False }
    in
        { model
            | lists = updatedLists
        }
            ! [ postAddContactsToLists model ]


postAddContactsToLists : Model -> Cmd Msg
postAddContactsToLists model =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/activities/add_list_memberships"

        listIds =
            List.map Json.Encode.string model.lists.selected

        contactIds =
            List.map Json.Encode.string model.contacts.selected

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

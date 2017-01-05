module ListActions exposing (..)

import Model exposing (..)
import Http
import EmailList exposing (..)
import HttpErrors
import HttpUtils exposing (..)
import Contact exposing (..)
import ContactActions
import Json.Encode


receive : Model -> EmailListResponse -> ( Model, Cmd Msg )
receive model response =
    let
        lists =
            model.lists

        updatedLists =
            { lists | elements = response.lists }
    in
        { model
            | lists = updatedLists
            , httpError = Nothing
        }
            ! []


showRenameModal : Model -> EmailList -> ( Model, Cmd Msg )
showRenameModal model list =
    let
        lists =
            model.lists

        updatedLists =
            { lists
                | showNameModal = True
                , active = Just list
                , httpAction = Put
                , displayedMenu = Nothing
            }
    in
        { model
            | lists = updatedLists
        }
            ! []


completeListRename : Model -> ( Model, Cmd Msg )
completeListRename model =
    let
        lists =
            model.lists

        updatedLists =
            { lists | showNameModal = False }
    in
        { model
            | httpError = Nothing
            , lists = updatedLists
        }
            ! [ getEmailLists ]


showNewListModal : Model -> ( Model, Cmd Msg )
showNewListModal model =
    let
        lists =
            model.lists

        updatedLists =
            { lists
                | showNameModal = True
                , httpAction = Post
                , displayedMenu = Nothing
            }
    in
        { model
            | lists = updatedLists
        }
            ! []


updateNewListName : Model -> String -> ( Model, Cmd Msg )
updateNewListName model name =
    let
        lists =
            model.lists

        updatedLists =
            { lists | newName = name }
    in
        { model
            | lists = updatedLists
        }
            ! []


listHttpError : Model -> Http.Error -> ( Model, Cmd Msg )
listHttpError model error =
    { model | httpError = Just error } ! []


requestListDelete : Model -> String -> ( Model, Cmd Msg )
requestListDelete model id =
    let
        lists =
            model.lists

        updatedLists =
            { lists | displayedMenu = Nothing }
    in
        { model
            | lists = updatedLists
        }
            ! [ deleteList id ]


submitListAction : Model -> ( Model, Cmd Msg )
submitListAction model =
    let
        id =
            case model.lists.active of
                Nothing ->
                    "bad"

                Just list ->
                    list.id
    in
        model
            ! [ listAction id model.lists.newName model.lists.httpAction ]


processListDelete : Model -> ( Model, Cmd Msg )
processListDelete model =
    let
        lists =
            model.lists

        updatedLists =
            { lists | active = Nothing }
    in
        { model
            | httpError = Nothing
            , lists = updatedLists
        }
            ! [ getEmailLists, ContactActions.getContacts All model.contacts.perPage ]


getEmailLists : Cmd Msg
getEmailLists =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists"
    in
        Http.send ProcessEmailLists (Http.get url emailListResponseDecoder)


closeListRenameModal : Model -> ( Model, Cmd Msg )
closeListRenameModal model =
    let
        lists =
            model.lists

        updatedLists =
            { lists | showNameModal = False }
    in
        { model
            | lists = updatedLists
            , httpError = Nothing
        }
            ! []


listAction : String -> String -> HttpAction -> Cmd Msg
listAction id newName action =
    let
        url =
            if action == Post then
                "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists"
            else
                "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists/" ++ id

        payload =
            Json.Encode.object
                [ ( "name", Json.Encode.string newName )
                , ( "favorite", Json.Encode.bool False )
                ]

        body =
            Http.stringBody "application/json" (Json.Encode.encode 0 payload)

        request =
            Http.request
                { method = httpActionToString action
                , headers = []
                , url = url
                , body = body
                , expect = Http.expectJson emailListDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send CompleteListRename request


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


setActiveListMenu : Model -> String -> ( Model, Cmd Msg )
setActiveListMenu model id =
    let
        lists =
            model.lists

        updatedLists =
            { lists | displayedMenu = Just id }
    in
        { model | lists = updatedLists } ! []

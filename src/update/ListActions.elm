module ListActions exposing (..)

import Model exposing (..)
import Http
import EmailList exposing (..)
import HttpErrors
import HttpUtils exposing (..)
import Contact exposing (..)
import ContactActions
import Json.Encode


processEmailLists : Model -> EmailListResponse -> ( Model, Cmd Msg )
processEmailLists model response =
    { model
        | lists = response.lists
        , httpError = Nothing
    }
        ! []


showRenameListModal : Model -> EmailList -> ( Model, Cmd Msg )
showRenameListModal model list =
    { model
        | showListNameModal = True
        , activeList = Just list
        , listMenuToShow = Nothing
        , listHttpAction = Put
    }
        ! []


completeListRename : Model -> ( Model, Cmd Msg )
completeListRename model =
    { model
        | httpError = Nothing
        , showListNameModal = False
    }
        ! [ getEmailLists ]


showNewListModal : Model -> ( Model, Cmd Msg )
showNewListModal model =
    { model
        | showListNameModal = True
        , listMenuToShow = Nothing
        , listHttpAction = Post
    }
        ! []


updateNewListName : Model -> String -> ( Model, Cmd Msg )
updateNewListName model name =
    { model
        | newListName = name
    }
        ! []


listHttpError : Model -> Http.Error -> ( Model, Cmd Msg )
listHttpError model error =
    { model | httpError = Just error } ! []


requestListDelete : Model -> String -> ( Model, Cmd Msg )
requestListDelete model id =
    { model
        | listMenuToShow = Nothing
    }
        ! [ deleteList id ]


submitListAction : Model -> ( Model, Cmd Msg )
submitListAction model =
    let
        id =
            case model.activeList of
                Nothing ->
                    "bad"

                Just list ->
                    list.id
    in
        model
            ! [ listAction id model.newListName model.listHttpAction ]


processListDelete : Model -> ( Model, Cmd Msg )
processListDelete model =
    { model
        | httpError = Nothing
        , activeList = Nothing
    }
        ! [ getEmailLists, ContactActions.getContacts All model.contactsPerPage ]


getEmailLists : Cmd Msg
getEmailLists =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists"
    in
        Http.send ProcessEmailLists (Http.get url emailListResponseDecoder)


closeListRenameModal : Model -> ( Model, Cmd Msg )
closeListRenameModal model =
    { model
        | showListNameModal = False
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
    { model | listMenuToShow = Just id } ! []

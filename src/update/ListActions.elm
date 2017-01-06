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
    (model
        |> setListElements response.lists
        |> setHttpError Nothing
    )
        ! []


showRenameModal : Model -> EmailList -> ( Model, Cmd Msg )
showRenameModal model list =
    (model
        |> setListShowNameModal True
        |> setListActive (Just list)
        |> setListHttpAction Put
        |> setListDisplayedMenu Nothing
    )
        ! []


completeListRename : Model -> ( Model, Cmd Msg )
completeListRename model =
    (model
        |> setListShowNameModal False
        |> setHttpError Nothing
    )
        ! [ getEmailLists ]


showNewListModal : Model -> ( Model, Cmd Msg )
showNewListModal model =
    (model
        |> setListShowNameModal True
        |> setListHttpAction Post
        |> setListDisplayedMenu Nothing
    )
        ! []


updateNewListName : Model -> String -> ( Model, Cmd Msg )
updateNewListName model name =
    (model
        |> setListNewName name
    )
        ! []


listHttpError : Model -> Http.Error -> ( Model, Cmd Msg )
listHttpError model error =
    (model
        |> setHttpError (Just error)
    )
        ! []


requestListDelete : Model -> String -> ( Model, Cmd Msg )
requestListDelete model id =
    (model
        |> setListDisplayedMenu Nothing
    )
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
    (model
        |> setListActive Nothing
        |> setHttpError Nothing
    )
        ! [ getEmailLists, ContactActions.getContacts All model.contacts.perPage ]


closeListRenameModal : Model -> ( Model, Cmd Msg )
closeListRenameModal model =
    (model
        |> setListShowNameModal False
        |> setHttpError Nothing
    )
        ! []


setActiveListMenu : Model -> String -> ( Model, Cmd Msg )
setActiveListMenu model id =
    (model
        |> setListDisplayedMenu (Just id)
    )
        ! []


getEmailLists : Cmd Msg
getEmailLists =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists"
    in
        Http.send ProcessEmailLists (Http.get url emailListResponseDecoder)


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

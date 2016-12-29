module Update exposing (..)

import Model exposing (..)
import Http
import Model exposing (..)
import Contact exposing (..)
import EmailList exposing (..)
import Tag exposing (..)
import HttpUtils exposing (..)
import Json.Decode
import Json.Encode
import HttpErrors
import TagActions exposing (..)
import ContactActions exposing (..)
import ListActions exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProcessContacts (Ok response) ->
            processContacts model response

        ProcessContacts (Err error) ->
            HttpErrors.setErrors model error

        GetContacts contactsFilterState ->
            requestContacts model contactsFilterState

        GetPaginatedContacts direction url ->
            requestPaginatedContacts model direction url

        DisplayContactsPerPageDropdown ->
            displayContactsPerPageDropdown model

        SetContactsPerPage count ->
            setContactsPerPage model count

        --
        ProcessEmailLists (Ok response) ->
            processEmailLists model response

        ProcessEmailLists (Err error) ->
            HttpErrors.setErrors model error

        ShowRenameListModal list ->
            showRenameListModal model list

        ShowNewListModal ->
            showNewListModal model

        CompleteListRename (Ok _) ->
            completeListRename model

        CompleteListRename (Err error) ->
            listHttpError model error

        CloseListRenameModal ->
            closeListRenameModal model

        UpdateNewListName name ->
            updateNewListName model name

        DeleteList id ->
            requestListDelete model id

        SubmitListAction ->
            submitListAction model

        ProcessListDelete (Ok _) ->
            processListDelete model

        ProcessListDelete (Err error) ->
            listHttpError model error

        SetActiveListMenu id ->
            setActiveListMenu model id

        --
        ShowAddContactsToListsModal ->
            showAddContactsToListModal model

        ProcessContactsCheckbox id state ->
            processContactsCheckbox model id state

        ProcessListCheckbox id state ->
            processListCheckbox model id state

        CloseAddContactsToListsModal ->
            closeAddContactsToListsModal model

        SubmitAddContactsToList ->
            submitAddContactsToList model

        CompleteAddContactsToList (Ok _) ->
            completeAddContactsToList model

        CompleteAddContactsToList (Err error) ->
            HttpErrors.setErrors model error

        --
        ProcessTags (Ok response) ->
            processTags model response

        ProcessTags (Err error) ->
            HttpErrors.setErrors model error


showAddContactsToListModal : Model -> ( Model, Cmd Msg )
showAddContactsToListModal model =
    { model
        | showAddContactsToListsModal = True
        , selectedLists = []
    }
        ! []


closeAddContactsToListsModal : Model -> ( Model, Cmd Msg )
closeAddContactsToListsModal model =
    { model
        | showAddContactsToListsModal = False
        , httpError = Nothing
    }
        ! []


requestListDelete : Model -> String -> ( Model, Cmd Msg )
requestListDelete model id =
    { model
        | listMenuToShow = Nothing
    }
        ! [ deleteList id ]


setActiveListMenu : Model -> String -> ( Model, Cmd Msg )
setActiveListMenu model id =
    { model | listMenuToShow = Just id } ! []


processContactsCheckbox : Model -> String -> Bool -> ( Model, Cmd Msg )
processContactsCheckbox model id state =
    if state == True then
        { model | selectedContacts = id :: model.selectedContacts } ! []
    else
        { model
            | selectedContacts =
                List.filter (\e -> e /= id) model.selectedContacts
        }
            ! []


processListCheckbox : Model -> String -> Bool -> ( Model, Cmd Msg )
processListCheckbox model id state =
    if state == True then
        { model | selectedLists = id :: model.selectedLists } ! []
    else
        { model
            | selectedLists =
                List.filter (\e -> e /= id) model.selectedLists
        }
            ! []



--
-- Contacts
--


completeAddContactsToList : Model -> ( Model, Cmd Msg )
completeAddContactsToList model =
    model ! []


submitAddContactsToList : Model -> ( Model, Cmd Msg )
submitAddContactsToList model =
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

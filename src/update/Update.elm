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


processContacts : Model -> ContactsResponse -> ( Model, Cmd Msg )
processContacts model response =
    let
        previous =
            if response.links.previous.url == "" then
                Nothing
            else
                Just response.links.previous.url

        next =
            if response.links.next.url == "" then
                Nothing
            else
                Just response.links.next.url
    in
        { model
            | contacts = response.contacts
            , selectedContacts = []
            , contactsCount = response.count
            , previousContactsUrl = previous
            , nextContactsUrl = next
            , httpError = Nothing
        }
            ! []


processEmailLists : Model -> EmailListResponse -> ( Model, Cmd Msg )
processEmailLists model response =
    { model
        | lists = response.lists
        , httpError = Nothing
    }
        ! []


processTags : Model -> TagsResponse -> ( Model, Cmd Msg )
processTags model response =
    { model
        | tags = response.tags
        , httpError = Nothing
    }
        ! []


requestContacts : Model -> ContactsFilterState -> ( Model, Cmd Msg )
requestContacts model contactsFilterState =
    { model
        | contacts = []
        , contactsCount = 0
        , startContactIndex = 1
        , contactsFilterState = contactsFilterState
    }
        ! [ getContacts contactsFilterState model.contactsPerPage ]


requestPaginatedContacts : Model -> PaginationDirection -> String -> ( Model, Cmd Msg )
requestPaginatedContacts model direction url =
    let
        increment =
            case direction of
                Forward ->
                    model.contactsPerPage

                Backward ->
                    0 - model.contactsPerPage
    in
        { model
            | startContactIndex = model.startContactIndex + increment
        }
            ! [ getPaginatedContacts url ]


setContactsPerPage : Model -> Int -> ( Model, Cmd Msg )
setContactsPerPage model count =
    { model
        | contactsPerPage = count
        , startContactIndex = 1
        , showContactsPerPageDropdown = False
    }
        ! [ getContacts model.contactsFilterState count ]


displayContactsPerPageDropdown : Model -> ( Model, Cmd Msg )
displayContactsPerPageDropdown model =
    { model | showContactsPerPageDropdown = True } ! []


showRenameListModal : Model -> EmailList -> ( Model, Cmd Msg )
showRenameListModal model list =
    { model
        | showListNameModal = True
        , activeList = Just list
        , listMenuToShow = Nothing
        , listHttpAction = Put
    }
        ! []


showAddContactsToListModal : Model -> ( Model, Cmd Msg )
showAddContactsToListModal model =
    { model
        | showAddContactsToListsModal = True
        , selectedLists = []
    }
        ! []


showNewListModal : Model -> ( Model, Cmd Msg )
showNewListModal model =
    { model
        | showListNameModal = True
        , listMenuToShow = Nothing
        , listHttpAction = Post
    }
        ! []


closeListRenameModal : Model -> ( Model, Cmd Msg )
closeListRenameModal model =
    { model
        | showListNameModal = False
        , httpError = Nothing
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


updateNewListName : Model -> String -> ( Model, Cmd Msg )
updateNewListName model name =
    { model
        | newListName = name
    }
        ! []


completeListRename : Model -> ( Model, Cmd Msg )
completeListRename model =
    { model
        | httpError = Nothing
        , showListNameModal = False
    }
        ! [ getEmailLists ]


listHttpError : Model -> Http.Error -> ( Model, Cmd Msg )
listHttpError model error =
    { model | httpError = Just error } ! []


processListDelete : Model -> ( Model, Cmd Msg )
processListDelete model =
    { model
        | httpError = Nothing
        , activeList = Nothing
    }
        ! [ getEmailLists, getContacts All model.contactsPerPage ]


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


completeAddContactsToList : Model -> ( Model, Cmd Msg )
completeAddContactsToList model =
    model ! []


submitAddContactsToList : Model -> ( Model, Cmd Msg )
submitAddContactsToList model =
    { model
        | showAddContactsToListsModal = False
    }
        ! [ postAddContactsToLists model ]



--
-- Lists
--
-- FIXME - want to pass in the favorite value. We now force false.


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


getEmailLists : Cmd Msg
getEmailLists =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists"
    in
        Http.send ProcessEmailLists (Http.get url emailListResponseDecoder)



--
-- Tags
--


getTags : Cmd Msg
getTags =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/tags"
    in
        Http.send ProcessTags (Http.get url tagsResponseDecoder)



--
-- Utilities
--


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

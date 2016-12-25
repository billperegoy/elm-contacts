module UpdateUtilities exposing (..)

import Model exposing (..)
import Http


errorString : Http.Error -> String
errorString error =
    case error of
        Http.BadStatus response ->
            "Bad Http Status: " ++ toString response.body

        _ ->
            toString error



--
-- Functions
--


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
            , contactsCount = response.count
            , previousContactsUrl = previous
            , nextContactsUrl = next
            , httpError = ""
        }
            ! []


processEmailLists : Model -> EmailListResponse -> ( Model, Cmd Msg )
processEmailLists model response =
    { model
        | lists = response.lists
        , httpError = ""
    }
        ! []


processTags : Model -> TagsResponse -> ( Model, Cmd Msg )
processTags model response =
    { model
        | tags = response.tags
        , httpError = ""
    }
        ! []


requestContacts : Model -> ContactsFilterState -> ( Model, Cmd Msg )
requestContacts model filterState =
    { model
        | contacts = []
        , contactsCount = 0
        , startContactIndex = 1
        , filterState = filterState
    }
        ! [ getContacts filterState model.contactsPerPage ]


setErrors : Model -> Http.Error -> ( Model, Cmd Msg )
setErrors model error =
    { model | httpError = toString error } ! []


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
        , displayContactsPerPageMenu = False
    }
        ! [ getContacts model.filterState count ]


displaySetContactsPerPageMenu : Model -> ( Model, Cmd Msg )
displaySetContactsPerPageMenu model =
    { model | displayContactsPerPageMenu = True } ! []


showRenameListModal : Model -> EmailList -> ( Model, Cmd Msg )
showRenameListModal model list =
    { model
        | showRenameModal = True
        , activeList = Just list
    }
        ! []


closeRenameModal : Model -> ( Model, Cmd Msg )
closeRenameModal model =
    { model
        | showRenameModal = False
        , httpError = ""
    }
        ! []


requestListDelete : Model -> String -> ( Model, Cmd Msg )
requestListDelete model id =
    model ! [ deleteList id ]


completeListRename : Model -> ( Model, Cmd Msg )
completeListRename model =
    let
        id =
            case model.activeList of
                Nothing ->
                    "bad"

                Just list ->
                    list.id
    in
        { model
            | showRenameModal = False
        }
            ! [ putList id model.newListName ]


updateNewListName : Model -> String -> ( Model, Cmd Msg )
updateNewListName model name =
    { model | newListName = name } ! []


processListPut : Model -> ( Model, Cmd Msg )
processListPut model =
    { model | httpError = "" } ! [ getEmailLists ]


listPutError : Model -> Http.Error -> ( Model, Cmd Msg )
listPutError model error =
    { model | httpError = errorString error } ! []


processListDelete : Model -> ( Model, Cmd Msg )
processListDelete model =
    { model
        | httpError = ""
        , activeList = Nothing
    }
        ! [ getEmailLists, getContacts All model.contactsPerPage ]

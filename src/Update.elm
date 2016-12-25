module Update exposing (..)

import Model exposing (..)
import Http


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProcessContacts (Ok response) ->
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

        ProcessEmailLists (Ok response) ->
            { model
                | lists = response.lists
                , httpError = ""
            }
                ! []

        ProcessTags (Ok response) ->
            { model
                | tags = response.tags
                , httpError = ""
            }
                ! []

        GetContacts filterState ->
            { model
                | contacts = []
                , contactsCount = 0
                , startContactIndex = 1
                , filterState = filterState
            }
                ! [ getContacts filterState model.contactsPerPage ]

        ProcessContacts (Err error) ->
            { model | httpError = toString error } ! []

        ProcessEmailLists (Err error) ->
            { model | httpError = toString error } ! []

        ProcessTags (Err error) ->
            { model | httpError = toString error } ! []

        GetPaginatedContacts direction url ->
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

        SetContactsPerPage count ->
            { model
                | contactsPerPage = count
                , startContactIndex = 1
                , displayContactsPerPageMenu = False
            }
                ! [ getContacts model.filterState count ]

        DisplaySetContactsPerPageMenu ->
            { model | displayContactsPerPageMenu = True } ! []

        ShowRenameListModal list ->
            { model
                | showRenameModal = True
                , activeList = Just list
            }
                ! []

        AcknowledgeDialog ->
            { model
                | showRenameModal = False
                , httpError = ""
            }
                ! []

        DeleteList id ->
            model ! [ deleteList id ]

        CompleteListRename ->
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

        UpdateNewListName name ->
            { model | newListName = name } ! []

        ProcessListPut (Ok result) ->
            { model | httpError = "" } ! [ getEmailLists ]

        ProcessListPut (Err error) ->
            { model | httpError = errorString error } ! []

        ProcessListDelete (Ok result) ->
            { model
                | httpError = ""
                , activeList = Nothing
            }
                ! [ getEmailLists, getContacts All model.contactsPerPage ]

        ProcessListDelete (Err error) ->
            { model | httpError = errorString error } ! []


errorString : Http.Error -> String
errorString error =
    case error of
        Http.BadStatus response ->
            "Bad Http Status: " ++ toString response.body

        _ ->
            toString error

module Update exposing (..)

import Model exposing (..)
import HttpErrors
import TagActions exposing (..)
import ContactActions exposing (..)
import ListActions exposing (..)
import AddContactsToListsActions exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --
        -- Contacts
        --
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
        -- Lists
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
        -- Add Contacts To Lists
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
        -- Tags
        --
        ProcessTags (Ok response) ->
            processTags model response

        ProcessTags (Err error) ->
            HttpErrors.setErrors model error

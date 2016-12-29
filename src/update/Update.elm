module Update exposing (..)

import Model exposing (..)
import HttpErrors
import TagActions
import ContactActions
import ListActions
import ContactListActions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --
        -- Contacts
        --
        ProcessContacts (Ok response) ->
            ContactActions.receive model response

        ProcessContacts (Err error) ->
            HttpErrors.setErrors model error

        GetContacts contactsFilterState ->
            ContactActions.request model contactsFilterState

        GetPaginatedContacts direction url ->
            ContactActions.requestPaginated model direction url

        DisplayContactsPerPageDropdown ->
            ContactActions.displayPerPageDropdown model

        SetContactsPerPage count ->
            ContactActions.setPerPageCount model count

        --
        -- Lists
        --
        ProcessEmailLists (Ok response) ->
            ListActions.receive model response

        ProcessEmailLists (Err error) ->
            HttpErrors.setErrors model error

        ShowRenameListModal list ->
            ListActions.showRenameModal model list

        ShowNewListModal ->
            ListActions.showNewListModal model

        CompleteListRename (Ok _) ->
            ListActions.completeListRename model

        CompleteListRename (Err error) ->
            ListActions.listHttpError model error

        CloseListRenameModal ->
            ListActions.closeListRenameModal model

        UpdateNewListName name ->
            ListActions.updateNewListName model name

        DeleteList id ->
            ListActions.requestListDelete model id

        SubmitListAction ->
            ListActions.submitListAction model

        ProcessListDelete (Ok _) ->
            ListActions.processListDelete model

        ProcessListDelete (Err error) ->
            ListActions.listHttpError model error

        SetActiveListMenu id ->
            ListActions.setActiveListMenu model id

        --
        -- Add Contacts To Lists
        --
        ShowAddContactsToListsModal ->
            ContactListActions.showModal model

        ProcessContactsCheckbox id state ->
            ContactListActions.contactsCheckbox model id state

        ProcessListCheckbox id state ->
            ContactListActions.listCheckbox model id state

        CloseAddContactsToListsModal ->
            ContactListActions.closeModal model

        SubmitAddContactsToList ->
            ContactListActions.submit model

        CompleteAddContactsToList (Ok _) ->
            ContactListActions.complete model

        CompleteAddContactsToList (Err error) ->
            HttpErrors.setErrors model error

        --
        -- Tags
        --
        ProcessTags (Ok response) ->
            TagActions.receive model response

        ProcessTags (Err error) ->
            HttpErrors.setErrors model error

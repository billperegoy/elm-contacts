module Update exposing (..)

import Model exposing (..)
import UpdateUtilities exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProcessContacts (Ok response) ->
            processContacts model response

        ProcessEmailLists (Ok response) ->
            processEmailLists model response

        ProcessTags (Ok response) ->
            processTags model response

        GetContacts contactsFilterState ->
            requestContacts model contactsFilterState

        ProcessContacts (Err error) ->
            setErrors model error

        ProcessEmailLists (Err error) ->
            setErrors model error

        ProcessTags (Err error) ->
            setErrors model error

        GetPaginatedContacts direction url ->
            requestPaginatedContacts model direction url

        SetContactsPerPage count ->
            setContactsPerPage model count

        DisplayContactsPerPageDropdown ->
            displayContactsPerPageDropdown model

        ShowRenameListModal list ->
            showRenameListModal model list

        CloseRenameModal ->
            closeRenameModal model

        DeleteList id ->
            requestListDelete model id

        SubmitListAction ->
            submitListAction model

        UpdateNewListName name ->
            updateNewListName model name

        CompleteListRename (Ok _) ->
            completeListRename model

        CompleteListRename (Err error) ->
            listHttpError model error

        ProcessListDelete (Ok _) ->
            processListDelete model

        ProcessListDelete (Err error) ->
            listHttpError model error

        SetActiveListMenu id ->
            setActiveListMenu model id

        ShowNewListModal ->
            showNewListModal model

        SetCheckbox id state ->
            setCheckbox model id state

        SetListCheckbox id state ->
            setListCheckbox model id state

        ShowAddToListsModal ->
            showAddToListModal model

        CloseAddToListsModal ->
            closeAddToListsModal model

        SubmitAddContactsToList ->
            { model | showAddToListsModal = False } ! []

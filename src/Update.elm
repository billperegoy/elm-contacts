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

        GetContacts filterState ->
            requestContacts model filterState

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

        DisplaySetContactsPerPageMenu ->
            displaySetContactsPerPageMenu model

        ShowRenameListModal list ->
            showRenameListModal model list

        CloseRenameModal ->
            closeRenameModal model

        DeleteList id ->
            requestListDelete model id

        CompleteListRename ->
            completeListRename model

        UpdateNewListName name ->
            updateNewListName model name

        ProcessListPut (Ok _) ->
            processListPut model

        ProcessListPut (Err error) ->
            listPutError model error

        ProcessListDelete (Ok _) ->
            processListDelete model

        ProcessListDelete (Err error) ->
            listPutError model error

        SetActiveListMenu id ->
            setActiveListMenu model id

        ShowNewListModal ->
            model ! []

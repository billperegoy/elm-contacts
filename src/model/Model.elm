module Model exposing (..)

import Http
import Contact exposing (..)
import EmailList exposing (..)
import Tag exposing (..)
import HttpUtils exposing (..)


type alias Model =
    { httpError : Maybe Http.Error
    , contacts : ContactsModel
    , lists : ListModel
    , tags : List Tag
    }


type alias ContactsModel =
    { elements : List Contact
    , count : Int
    , startIndex : Int
    , nextUrl : Maybe String
    , previousUrl : Maybe String
    , filterState : ContactsFilterState
    , perPage : Int
    , perPageDropdown : Bool
    , selected : List String
    }


type alias ListModel =
    { elements : List EmailList
    , showNameModal : Bool
    , active : Maybe EmailList
    , newName : String
    , httpAction : HttpAction
    , displayedMenu : Maybe String
    , showAddContactsToListsModal : Bool
    , selected : List String
    }


setContactElements : List Contact -> Model -> Model
setContactElements elements model =
    let
        contacts =
            model.contacts

        updatedContacts =
            { contacts | elements = elements }
    in
        { model | contacts = updatedContacts }


setContactCount : Int -> Model -> Model
setContactCount count model =
    let
        contacts =
            model.contacts

        updatedContacts =
            { contacts | count = count }
    in
        { model | contacts = updatedContacts }


setContactPreviousUrl : Maybe String -> Model -> Model
setContactPreviousUrl previousUrl model =
    let
        contacts =
            model.contacts

        updatedContacts =
            { contacts | previousUrl = previousUrl }
    in
        { model | contacts = updatedContacts }


setContactNextUrl : Maybe String -> Model -> Model
setContactNextUrl nextUrl model =
    let
        contacts =
            model.contacts

        updatedContacts =
            { contacts | nextUrl = nextUrl }
    in
        { model | contacts = updatedContacts }


setContactSelected : List String -> Model -> Model
setContactSelected selected model =
    let
        contacts =
            model.contacts

        updatedContacts =
            { contacts | selected = selected }
    in
        { model | contacts = updatedContacts }


setContactStartIndex : Int -> Model -> Model
setContactStartIndex startIndex model =
    let
        contacts =
            model.contacts

        updatedContacts =
            { contacts | startIndex = startIndex }
    in
        { model | contacts = updatedContacts }


setContactFilterState : ContactsFilterState -> Model -> Model
setContactFilterState filterState model =
    let
        contacts =
            model.contacts

        updatedContacts =
            { contacts | filterState = filterState }
    in
        { model | contacts = updatedContacts }


setContactPerPage : Int -> Model -> Model
setContactPerPage perPage model =
    let
        contacts =
            model.contacts

        updatedContacts =
            { contacts | perPage = perPage }
    in
        { model | contacts = updatedContacts }


setContactPerPageDropdown : Bool -> Model -> Model
setContactPerPageDropdown perPageDropdown model =
    let
        contacts =
            model.contacts

        updatedContacts =
            { contacts | perPageDropdown = perPageDropdown }
    in
        { model | contacts = updatedContacts }


setHttpError : Maybe Http.Error -> Model -> Model
setHttpError httpError model =
    { model | httpError = httpError }


initContacts : ContactsModel
initContacts =
    { elements = []
    , count = 0
    , startIndex = 1
    , nextUrl = Nothing
    , previousUrl = Nothing
    , filterState = All
    , perPage = 50
    , perPageDropdown = False
    , selected = []
    }


initLists : ListModel
initLists =
    { elements = []
    , showNameModal = False
    , active = Nothing
    , newName = ""
    , httpAction = Get
    , displayedMenu = Nothing
    , showAddContactsToListsModal = False
    , selected = []
    }


init : Model
init =
    { contacts = initContacts
    , httpError = Nothing
    , tags = []
    , lists = initLists
    }


type Msg
    = ProcessContacts (Result Http.Error ContactsResponse)
    | GetContacts ContactsFilterState
    | GetPaginatedContacts PaginationDirection String
    | DisplayContactsPerPageDropdown
    | SetContactsPerPage Int
      --
    | ProcessEmailLists (Result Http.Error EmailListResponse)
    | ShowRenameListModal EmailList
    | ShowNewListModal
    | CompleteListRename (Result Http.Error EmailList)
    | CloseListRenameModal
    | UpdateNewListName String
    | DeleteList String
    | SubmitListAction
    | ProcessListDelete (Result Http.Error MassActionResponse)
    | SetActiveListMenu String
      --
    | ShowAddContactsToListsModal
    | ProcessContactsCheckbox String Bool
    | ProcessListCheckbox String Bool
    | CloseAddContactsToListsModal
    | SubmitAddContactsToList
    | CompleteAddContactsToList (Result Http.Error MassActionResponse)
      --
    | ProcessTags (Result Http.Error TagsResponse)

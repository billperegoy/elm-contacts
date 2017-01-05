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

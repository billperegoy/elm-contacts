module Model exposing (..)

import Http
import Contact exposing (..)
import EmailList exposing (..)
import Tag exposing (..)
import HttpUtils exposing (..)


type alias Model =
    { httpError : Maybe Http.Error
    , contacts : List Contact
    , selectedContacts : List String
    , contactsCount : Int
    , showContactsPerPageDropdown : Bool
    , contactsPerPage : Int
    , startContactIndex : Int
    , nextContactsUrl : Maybe String
    , previousContactsUrl : Maybe String
    , contactsFilterState : ContactsFilterState
    , lists : List EmailList
    , selectedLists : List String
    , showListNameModal : Bool
    , activeList : Maybe EmailList
    , newListName : String
    , listMenuToShow : Maybe String
    , listHttpAction : HttpAction
    , tags : List Tag
    , showAddContactsToListsModal : Bool
    }


init : Model
init =
    { contactsCount = 0
    , contactsPerPage = 50
    , showContactsPerPageDropdown = False
    , contacts = []
    , selectedContacts = []
    , selectedLists = []
    , startContactIndex = 1
    , nextContactsUrl = Nothing
    , previousContactsUrl = Nothing
    , contactsFilterState = All
    , httpError = Nothing
    , tags = []
    , lists = []
    , listHttpAction = Get
    , showListNameModal = False
    , activeList = Nothing
    , newListName = ""
    , listMenuToShow = Nothing
    , showAddContactsToListsModal = False
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

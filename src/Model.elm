module Model exposing (..)

import Http
import Contact exposing (..)
import EmailList exposing (..)
import Tag exposing (..)
import HttpUtils exposing (..)


type alias Model =
    { contacts : List Contact
    , selectedContacts : List String
    , contactsCount : Int
    , showContactsPerPageDropdown : Bool
    , contactsPerPage : Int
    , startContactIndex : Int
    , nextContactsUrl : Maybe String
    , previousContactsUrl : Maybe String
    , contactsFilterState : ContactsFilterState
    , tags : List Tag
    , httpError : Maybe Http.Error
    , lists : List EmailList
    , showListNameModal : Bool
    , activeList : Maybe EmailList
    , newListName : String
    , listMenuToShow : Maybe String
    , listHttpAction : HttpAction
    }


type Msg
    = ProcessContacts (Result Http.Error ContactsResponse)
    | ProcessEmailLists (Result Http.Error EmailListResponse)
    | ProcessTags (Result Http.Error TagsResponse)
    | GetContacts ContactsFilterState
    | GetPaginatedContacts PaginationDirection String
    | DisplayContactsPerPageDropdown
    | SetContactsPerPage Int
    | ShowRenameListModal EmailList
    | ShowNewListModal
    | UpdateNewListName String
    | DeleteList String
    | SubmitListAction
    | CloseRenameModal
    | CompleteListRename (Result Http.Error EmailList)
    | ProcessListDelete (Result Http.Error DeleteResponse)
    | SetActiveListMenu String
    | SetCheckbox String Bool

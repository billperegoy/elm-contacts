module Model exposing (..)

import Http
import Contact exposing (..)
import EmailList exposing (..)
import Tag exposing (..)
import HttpUtils exposing (..)


--
-- Model
--


type alias Model =
    { contacts : List Contact
    , contactsCount : Int
    , displayContactsPerPageMenu : Bool
    , contactsPerPage : Int
    , startContactIndex : Int
    , nextContactsUrl : Maybe String
    , previousContactsUrl : Maybe String
    , contactsFilterState : ContactsFilterState
    , listHttpAction : String
    , tags : List Tag
    , httpError : String
    , showRenameModal : Bool
    , lists : List EmailList
    , activeList : Maybe EmailList
    , newListName : String
    , activeListMenu : Maybe String
    }


type Msg
    = ProcessContacts (Result Http.Error ContactsResponse)
    | ProcessEmailLists (Result Http.Error EmailListResponse)
    | ProcessTags (Result Http.Error TagsResponse)
    | GetContacts ContactsFilterState
    | GetPaginatedContacts PaginationDirection String
    | DisplaySetContactsPerPageMenu
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

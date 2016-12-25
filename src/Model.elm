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
    { contactsCount : Int
    , displayContactsPerPageMenu : Bool
    , contactsPerPage : Int
    , contacts : List Contact
    , startContactIndex : Int
    , nextContactsUrl : Maybe String
    , previousContactsUrl : Maybe String
    , filterState : ContactsFilterState
    , tags : List Tag
    , lists : List EmailList
    , httpError : String
    , showRenameModal : Bool
    , activeList : Maybe EmailList
    , newListName : String
    }



--
-- Msg
--


type Msg
    = ProcessContacts (Result Http.Error ContactsResponse)
    | ProcessEmailLists (Result Http.Error EmailListResponse)
    | ProcessTags (Result Http.Error TagsResponse)
    | GetContacts ContactsFilterState
    | GetPaginatedContacts PaginationDirection String
    | DisplaySetContactsPerPageMenu
    | SetContactsPerPage Int
    | ShowRenameListModal EmailList
    | UpdateNewListName String
    | DeleteList String
    | CompleteListRename
    | CloseRenameModal
    | ProcessListPut (Result Http.Error EmailList)
    | ProcessListDelete (Result Http.Error DeleteResponse)
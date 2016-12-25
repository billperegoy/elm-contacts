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



{-
   init : ( Model, Cmd Msg )
   init =
       let
           contactsPerPage =
               50
       in
           { contactsCount = 0
           , contactsPerPage = contactsPerPage
           , displayContactsPerPageMenu = False
           , contacts = []
           , startContactIndex = 1
           , nextContactsUrl = Nothing
           , previousContactsUrl = Nothing
           , filterState = All
           , tags = []
           , lists = []
           , httpError = ""
           , showRenameModal = False
           , activeList = Nothing
           , newListName = ""
           }
               ! [ getContacts All contactsPerPage, getEmailLists, getTags ]



   --
   -- Contacts
   --


   getContacts : ContactsFilterState -> Int -> Cmd Msg
   getContacts filter contactsPerPage =
       let
           baseUrl =
               "http://0.0.0.0:3000/contacts-service/v3/accounts/1/contacts"

           sortParam =
               ( "sort", "contacts.last_name" )

           limitParam =
               ( "limit", toString contactsPerPage )

           countParam =
               ( "include_count", "true" )

           commonParams =
               [ sortParam, countParam, limitParam ]

           url =
               case filter of
                   All ->
                       urlString baseUrl commonParams

                   Unsubscribed ->
                       urlString baseUrl (( "status", "unsubscribed" ) :: commonParams)

                   ByTag id ->
                       urlString baseUrl (( "tags", id ) :: commonParams)

                   ByList id ->
                       urlString baseUrl (( "lists", id ) :: commonParams)
       in
           Http.send ProcessContacts (Http.get url contactResponseDecoder)


   getPaginatedContacts : String -> Cmd Msg
   getPaginatedContacts path =
       let
           url =
               "http://0.0.0.0:3000/" ++ path
       in
           Http.send ProcessContacts (Http.get url contactResponseDecoder)



   --
   -- Lists
   --


   putList : String -> String -> Cmd Msg
   putList id newName =
       let
           url =
               "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists/" ++ id

           body =
               Http.stringBody "application/json"
                   ("""{"name" : """ ++ "\"" ++ newName ++ "\"" ++ """, "favorite" : "false"}""")

           request =
               Http.request
                   { method = "PUT"
                   , headers = []
                   , url = url
                   , body = body
                   , expect = Http.expectJson emailListDecoder
                   , timeout = Nothing
                   , withCredentials = False
                   }
       in
           Http.send ProcessListPut request


   deleteList : String -> Cmd Msg
   deleteList id =
       let
           url =
               "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists/" ++ id

           request =
               Http.request
                   { method = "DELETE"
                   , headers = []
                   , url = url
                   , body = Http.emptyBody
                   , expect = Http.expectJson deleteResponseDecoder
                   , timeout = Nothing
                   , withCredentials = False
                   }
       in
           Http.send ProcessListDelete request


   getEmailLists : Cmd Msg
   getEmailLists =
       let
           url =
               "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists"
       in
           Http.send ProcessEmailLists (Http.get url emailListResponseDecoder)



   --
   -- Tags
   --


   getTags : Cmd Msg
   getTags =
       let
           url =
               "http://0.0.0.0:3000/contacts-service/v3/accounts/1/tags"
       in
           Http.send ProcessTags (Http.get url tagsResponseDecoder)



   --
   -- Utilities
   --


   httpParams : List ( String, String ) -> String
   httpParams params =
       let
           paramStringList =
               List.map (\( name, value ) -> name ++ "=" ++ value) params
       in
           String.join "&" paramStringList


   urlString : String -> List ( String, String ) -> String
   urlString baseUrl params =
       baseUrl
           ++ "?"
           ++ httpParams params
-}

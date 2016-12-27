module UpdateUtilities exposing (..)

import Http
import Model exposing (..)
import Contact exposing (..)
import EmailList exposing (..)
import Tag exposing (..)
import HttpUtils exposing (..)
import Json.Decode


processContacts : Model -> ContactsResponse -> ( Model, Cmd Msg )
processContacts model response =
    let
        previous =
            if response.links.previous.url == "" then
                Nothing
            else
                Just response.links.previous.url

        next =
            if response.links.next.url == "" then
                Nothing
            else
                Just response.links.next.url
    in
        { model
            | contacts = response.contacts
            , contactsCount = response.count
            , previousContactsUrl = previous
            , nextContactsUrl = next
            , httpError = Nothing
        }
            ! []


processEmailLists : Model -> EmailListResponse -> ( Model, Cmd Msg )
processEmailLists model response =
    { model
        | lists = response.lists
        , httpError = Nothing
    }
        ! []


processTags : Model -> TagsResponse -> ( Model, Cmd Msg )
processTags model response =
    { model
        | tags = response.tags
        , httpError = Nothing
    }
        ! []


requestContacts : Model -> ContactsFilterState -> ( Model, Cmd Msg )
requestContacts model contactsFilterState =
    { model
        | contacts = []
        , contactsCount = 0
        , startContactIndex = 1
        , contactsFilterState = contactsFilterState
    }
        ! [ getContacts contactsFilterState model.contactsPerPage ]


setErrors : Model -> Http.Error -> ( Model, Cmd Msg )
setErrors model error =
    { model | httpError = Just error } ! []


requestPaginatedContacts : Model -> PaginationDirection -> String -> ( Model, Cmd Msg )
requestPaginatedContacts model direction url =
    let
        increment =
            case direction of
                Forward ->
                    model.contactsPerPage

                Backward ->
                    0 - model.contactsPerPage
    in
        { model
            | startContactIndex = model.startContactIndex + increment
        }
            ! [ getPaginatedContacts url ]


setContactsPerPage : Model -> Int -> ( Model, Cmd Msg )
setContactsPerPage model count =
    { model
        | contactsPerPage = count
        , startContactIndex = 1
        , showContactsPerPageDropdown = False
    }
        ! [ getContacts model.contactsFilterState count ]


displayContactsPerPageDropdown : Model -> ( Model, Cmd Msg )
displayContactsPerPageDropdown model =
    { model | showContactsPerPageDropdown = True } ! []


showRenameListModal : Model -> EmailList -> ( Model, Cmd Msg )
showRenameListModal model list =
    { model
        | showListNameModal = True
        , activeList = Just list
        , listMenuToShow = Nothing
        , listHttpAction = Put
    }
        ! []


showNewListModal : Model -> ( Model, Cmd Msg )
showNewListModal model =
    { model
        | showListNameModal = True
        , listMenuToShow = Nothing
        , listHttpAction = Post
    }
        ! []


closeRenameModal : Model -> ( Model, Cmd Msg )
closeRenameModal model =
    { model
        | showListNameModal = False
        , httpError = Nothing
    }
        ! []


requestListDelete : Model -> String -> ( Model, Cmd Msg )
requestListDelete model id =
    { model
        | listMenuToShow = Nothing
    }
        ! [ deleteList id ]


submitListAction : Model -> ( Model, Cmd Msg )
submitListAction model =
    let
        id =
            case model.activeList of
                Nothing ->
                    "bad"

                Just list ->
                    list.id
    in
        model
            ! [ listAction id model.newListName model.listHttpAction ]


updateNewListName : Model -> String -> ( Model, Cmd Msg )
updateNewListName model name =
    { model
        | newListName = name
    }
        ! []


completeListRename : Model -> ( Model, Cmd Msg )
completeListRename model =
    { model
        | httpError = Nothing
        , showListNameModal = False
    }
        ! [ getEmailLists ]


listHttpError : Model -> Http.Error -> ( Model, Cmd Msg )
listHttpError model error =
    { model | httpError = Just error } ! []


processListDelete : Model -> ( Model, Cmd Msg )
processListDelete model =
    { model
        | httpError = Nothing
        , activeList = Nothing
    }
        ! [ getEmailLists, getContacts All model.contactsPerPage ]


setActiveListMenu : Model -> String -> ( Model, Cmd Msg )
setActiveListMenu model id =
    { model | listMenuToShow = Just id } ! []



--
-- init
--


init : ( Model, Cmd Msg )
init =
    let
        contactsPerPage =
            50
    in
        { contactsCount = 0
        , contactsPerPage = contactsPerPage
        , showContactsPerPageDropdown = False
        , contacts = []
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
-- FIXME - want to pass in the favorite value. We now force false.


listAction : String -> String -> HttpAction -> Cmd Msg
listAction id newName action =
    let
        url =
            if action == Post then
                "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists"
            else
                "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists/" ++ id

        body =
            Http.stringBody "application/json"
                ("""{"name" : """ ++ "\"" ++ newName ++ "\"" ++ """, "favorite" : "false"}""")

        request =
            Http.request
                { method = httpActionToString action
                , headers = []
                , url = url
                , body = body
                , expect = Http.expectJson emailListDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send CompleteListRename request


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

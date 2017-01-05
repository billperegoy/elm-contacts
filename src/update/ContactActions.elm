module ContactActions exposing (..)

import Model exposing (..)
import Http
import Contact exposing (..)
import HttpErrors


receive : Model -> ContactsResponse -> ( Model, Cmd Msg )
receive model response =
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

        contacts =
            model.contacts

        updatedContacts =
            { contacts
                | elements = response.contacts
                , count = response.count
                , previousUrl = previous
                , nextUrl = next
                , selected = []
            }
    in
        { model
            | contacts = updatedContacts
            , httpError = Nothing
        }
            ! []


request : Model -> ContactsFilterState -> ( Model, Cmd Msg )
request model contactsFilterState =
    let
        contacts =
            model.contacts

        updatedContacts =
            { contacts
                | elements = []
                , count = 0
                , startIndex = 1
                , filterState = contactsFilterState
            }
    in
        { model
            | contacts = updatedContacts
        }
            ! [ getContacts contactsFilterState model.contacts.perPage ]


requestPaginated : Model -> PaginationDirection -> String -> ( Model, Cmd Msg )
requestPaginated model direction url =
    let
        increment =
            case direction of
                Forward ->
                    model.contacts.perPage

                Backward ->
                    0 - model.contacts.perPage

        contacts =
            model.contacts

        updatedContacts =
            { contacts
                | startIndex = contacts.startIndex + increment
            }
    in
        { model
            | contacts = updatedContacts
        }
            ! [ getPaginatedContacts url ]


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


displayPerPageDropdown : Model -> ( Model, Cmd Msg )
displayPerPageDropdown model =
    let
        contacts =
            model.contacts

        updatedContacts =
            { contacts | perPageDropdown = True }
    in
        { model | contacts = updatedContacts } ! []


setPerPageCount : Model -> Int -> ( Model, Cmd Msg )
setPerPageCount model count =
    let
        contacts =
            model.contacts

        updatedContacts =
            { contacts
                | startIndex = 1
                , perPage = count
                , perPageDropdown = False
            }
    in
        { model
            | contacts = updatedContacts
        }
            ! [ getContacts model.contacts.filterState count ]

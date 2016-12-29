module ContactActions exposing (..)

import Model exposing (..)
import Http
import Contact exposing (..)
import HttpErrors


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
            , selectedContacts = []
            , contactsCount = response.count
            , previousContactsUrl = previous
            , nextContactsUrl = next
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


displayContactsPerPageDropdown : Model -> ( Model, Cmd Msg )
displayContactsPerPageDropdown model =
    { model | showContactsPerPageDropdown = True } ! []


setContactsPerPage : Model -> Int -> ( Model, Cmd Msg )
setContactsPerPage model count =
    { model
        | contactsPerPage = count
        , startContactIndex = 1
        , showContactsPerPageDropdown = False
    }
        ! [ getContacts model.contactsFilterState count ]

module ContactActions exposing (..)

import Model exposing (..)
import Http
import Contact exposing (..)
import HttpErrors


convertUrl : String -> Maybe String
convertUrl url =
    if url == "" then
        Nothing
    else
        Just url


indexOffset : Model -> PaginationDirection -> Int
indexOffset model direction =
    case direction of
        Forward ->
            model.contacts.perPage

        Backward ->
            0 - model.contacts.perPage


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


receive : Model -> ContactsResponse -> ( Model, Cmd Msg )
receive model response =
    (model
        |> setContactElements response.contacts
        |> setContactCount response.count
        |> setContactPreviousUrl (convertUrl response.links.previous.url)
        |> setContactNextUrl (convertUrl response.links.next.url)
        |> setContactSelected []
        |> setHttpError Nothing
    )
        ! []


request : Model -> ContactsFilterState -> ( Model, Cmd Msg )
request model contactsFilterState =
    (model
        |> setContactElements []
        |> setContactCount 0
        |> setContactStartIndex 1
        |> setContactFilterState contactsFilterState
    )
        ! [ getContacts contactsFilterState model.contacts.perPage ]


requestPaginated : Model -> PaginationDirection -> String -> ( Model, Cmd Msg )
requestPaginated model direction url =
    let
        offset =
            indexOffset model direction
    in
        (model
            |> setContactStartIndex (model.contacts.startIndex + offset)
        )
            ! [ getPaginatedContacts url ]


displayPerPageDropdown : Model -> ( Model, Cmd Msg )
displayPerPageDropdown model =
    (model |> setContactPerPageDropdown True)
        ! []


setPerPageCount : Model -> Int -> ( Model, Cmd Msg )
setPerPageCount model count =
    (model
        |> setContactStartIndex 1
        |> setContactPerPage count
        |> setContactPerPageDropdown False
    )
        ! [ getContacts model.contacts.filterState count ]


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

module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Json.Decode.Pipeline
import Dialog


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


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


type alias ContactsResponse =
    { count : Int
    , contacts : List Contact
    , links : Links
    }


type alias Contact =
    { firstName : Maybe String
    , lastName : Maybe String
    , email : Email
    }


type alias Email =
    { address : String
    }


type alias EmailListResponse =
    { lists : List EmailList
    }


type alias EmailList =
    { id : String
    , name : String
    }


type alias TagsResponse =
    { tags : List Tag
    }


type alias Tag =
    { id : String
    , name : String
    }


type ContactsFilterState
    = All
    | Unsubscribed
    | ByTag String
    | ByList String


type PaginationDirection
    = Forward
    | Backward


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



-- Update


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
    | AcknowledgeDialog
    | ProcessListPut (Result Http.Error EmailList)
    | ProcessListDelete (Result Http.Error DeleteResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProcessContacts (Ok response) ->
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
                    , httpError = ""
                }
                    ! []

        ProcessEmailLists (Ok response) ->
            { model
                | lists = response.lists
                , httpError = ""
            }
                ! []

        ProcessTags (Ok response) ->
            { model
                | tags = response.tags
                , httpError = ""
            }
                ! []

        GetContacts filterState ->
            { model
                | contacts = []
                , contactsCount = 0
                , startContactIndex = 1
                , filterState = filterState
            }
                ! [ getContacts filterState model.contactsPerPage ]

        ProcessContacts (Err error) ->
            { model | httpError = toString error } ! []

        ProcessEmailLists (Err error) ->
            { model | httpError = toString error } ! []

        ProcessTags (Err error) ->
            { model | httpError = toString error } ! []

        GetPaginatedContacts direction url ->
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

        SetContactsPerPage count ->
            { model
                | contactsPerPage = count
                , startContactIndex = 1
                , displayContactsPerPageMenu = False
            }
                ! [ getContacts model.filterState count ]

        DisplaySetContactsPerPageMenu ->
            { model | displayContactsPerPageMenu = True } ! []

        ShowRenameListModal list ->
            { model
                | showRenameModal = True
                , activeList = Just list
            }
                ! []

        AcknowledgeDialog ->
            { model
                | showRenameModal = False
                , httpError = ""
            }
                ! []

        DeleteList id ->
            model ! [ deleteList id ]

        CompleteListRename ->
            let
                id =
                    case model.activeList of
                        Nothing ->
                            "bad"

                        Just list ->
                            list.id
            in
                { model
                    | showRenameModal = False
                }
                    ! [ putList id model.newListName ]

        UpdateNewListName name ->
            { model | newListName = name } ! []

        ProcessListPut (Ok result) ->
            { model | httpError = "" } ! [ getEmailLists ]

        ProcessListPut (Err error) ->
            { model | httpError = errorString error } ! []

        ProcessListDelete (Ok result) ->
            { model
                | httpError = ""
                , activeList = Nothing
            }
                ! [ getEmailLists, getContacts All model.contactsPerPage ]

        ProcessListDelete (Err error) ->
            { model | httpError = errorString error } ! []


errorString : Http.Error -> String
errorString error =
    case error of
        Http.BadStatus response ->
            "Bad Http Status: " ++ toString response.body

        _ ->
            toString error



-- View


tableHeader : Html Msg
tableHeader =
    thead []
        [ tr []
            [ th [] [ text "Last Name" ]
            , th [] [ text "First Name" ]
            , th [] [ text "Email" ]
            ]
        ]


tableBody : List Contact -> Html Msg
tableBody contacts =
    tbody []
        (contactRows contacts)


contactsTable : List Contact -> Html Msg
contactsTable contacts =
    table [ class "table table-striped" ]
        [ tableHeader
        , tableBody contacts
        ]


contactRows : List Contact -> List (Html Msg)
contactRows contacts =
    List.map
        (\contact ->
            tr []
                [ td [] [ text (Maybe.withDefault "" contact.lastName) ]
                , td [] [ text (Maybe.withDefault "" contact.firstName) ]
                , td [] [ text contact.email.address ]
                ]
        )
        contacts


tagName : String -> List Tag -> String
tagName id tags =
    tags
        |> List.filter (\tag -> tag.id == id)
        |> List.head
        |> Maybe.withDefault { name = "unknown", id = "inknown" }
        |> .name


listName : String -> List EmailList -> String
listName id lists =
    lists
        |> List.filter (\list -> list.id == id)
        |> List.head
        |> Maybe.withDefault { name = "unknown", id = "inknown" }
        |> .name


contactsCount : Model -> Html Msg
contactsCount model =
    let
        displayText =
            case model.filterState of
                All ->
                    "All Contacts (" ++ toString model.contactsCount ++ ")"

                Unsubscribed ->
                    "Unsubscribed (" ++ toString model.contactsCount ++ ")"

                ByTag id ->
                    tagName id model.tags ++ " (" ++ toString model.contactsCount ++ ")"

                ByList id ->
                    listName id model.lists ++ " (" ++ toString model.contactsCount ++ ")"
    in
        h2 []
            [ span [ class "label label-primary" ]
                [ text displayText ]
            ]


errors : String -> Html Msg
errors errorString =
    if errorString == "" then
        div [] []
    else
        div [ class "alert alert-danger" ] [ text errorString ]


sidebarContacts : Html Msg
sidebarContacts =
    div []
        [ h4 []
            [ span [ class "label label-success" ] [ text "contacts" ]
            ]
        , ul []
            [ li [] [ text "active" ]
            , li []
                [ a
                    [ onClick (GetContacts Unsubscribed), href "#" ]
                    [ text "unsubscribed" ]
                ]
            , li []
                [ a
                    [ onClick (GetContacts All), href "#" ]
                    [ text "view all contacts" ]
                ]
            ]
        ]


sidebarLink : Msg -> String -> Html Msg
sidebarLink msg label =
    li []
        [ a
            [ style
                [ ( "margin-left", "7px" )
                ]
            , href "#"
            , onClickNoDefault msg
            ]
            [ text label ]
        ]


sidebarLists : List EmailList -> Html Msg
sidebarLists lists =
    let
        listElement list =
            li []
                [ a [ onClick (GetContacts (ByList list.id)), href "#" ] [ text list.name ]
                , ul []
                    [ sidebarLink (ShowRenameListModal list) "rename"
                    , sidebarLink (DeleteList list.id) "delete"
                    ]
                ]
    in
        div []
            [ h4 [] [ span [ class "label label-success" ] [ text "email lists" ] ]
            , ul [] (List.map (\list -> listElement list) lists)
            ]


sidebarTags : List Tag -> Html Msg
sidebarTags tags =
    let
        tagElement tag =
            li []
                [ a [ onClick (GetContacts (ByTag tag.id)), href "#" ] [ text tag.name ] ]
    in
        div []
            [ h4 [] [ span [ class "label label-success" ] [ text "tags" ] ]
            , ul [] (List.map (\tag -> tagElement tag) tags)
            ]


sidebar : List EmailList -> List Tag -> Html Msg
sidebar lists tags =
    div [ class "col-md-3" ]
        [ sidebarContacts
        , sidebarLists lists
        , sidebarTags tags
        ]


mainContent : Model -> Html Msg
mainContent model =
    div [ class "col-md-9" ]
        [ (errors model.httpError)
        , (contactsCount model)
        , (contactsTable model.contacts)
        , (setContactsPerPage model)
        , (pagination model)
        ]


setContactsPerPage : Model -> Html Msg
setContactsPerPage model =
    let
        legalValues =
            [ 50, 100, 250, 500 ]

        displayString value =
            "Show " ++ toString value ++ " rows per page"

        contactListElement value =
            li [] [ a [ href "#", onClickNoDefault (SetContactsPerPage value) ] [ text (displayString value) ] ]

        menuHeader =
            a [ href "#", onClickNoDefault DisplaySetContactsPerPageMenu ]
                [ text (displayString model.contactsPerPage) ]

        menu =
            if model.displayContactsPerPageMenu then
                ul [ style [ ( "list-style-type", "none" ) ] ]
                    (List.map (\value -> contactListElement value) legalValues)
            else
                p [] []
    in
        div []
            [ menuHeader
            , menu
            ]


pagination : Model -> Html Msg
pagination model =
    let
        startIndex =
            toString model.startContactIndex

        endIndex =
            if (model.startContactIndex + model.contactsPerPage - 1) > model.contactsCount then
                toString model.contactsCount
            else
                toString (model.startContactIndex + model.contactsPerPage - 1)

        nextLinkUrl =
            Maybe.withDefault "" model.nextContactsUrl

        previousLinkUrl =
            Maybe.withDefault "" model.previousContactsUrl

        nextLink =
            if nextLinkUrl == "" then
                span [] []
            else
                span [ style [ ( "margin-right", "5px" ) ] ]
                    [ a [ href "#", onClick (GetPaginatedContacts Forward nextLinkUrl) ]
                        [ span [ class "glyphicon glyphicon-step-forward" ] [] ]
                    ]

        previousLink =
            if previousLinkUrl == "" then
                span [] []
            else
                span [ style [ ( "margin-right", "5px" ) ] ]
                    [ a [ href "#", onClick (GetPaginatedContacts Backward previousLinkUrl) ]
                        [ span [ class "glyphicon glyphicon-step-backward" ] [] ]
                    ]
    in
        div []
            [ previousLink
            , span [] [ text (startIndex ++ "-" ++ endIndex ++ " of " ++ toString model.contactsCount) ]
            , nextLink
            ]


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ div
            [ class "row" ]
            [ (sidebar model.lists model.tags)
            , (mainContent model)
            , (renameModal model)
            ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Http


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


getEmailLists : Cmd Msg
getEmailLists =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/lists"
    in
        Http.send ProcessEmailLists (Http.get url emailListResponseDecoder)


getTags : Cmd Msg
getTags =
    let
        url =
            "http://0.0.0.0:3000/contacts-service/v3/accounts/1/tags"
    in
        Http.send ProcessTags (Http.get url tagsResponseDecoder)


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


type alias DeleteResponse =
    { activityId : String
    }


deleteResponseDecoder : Json.Decode.Decoder DeleteResponse
deleteResponseDecoder =
    Json.Decode.Pipeline.decode DeleteResponse
        |> Json.Decode.Pipeline.required "activity_id" Json.Decode.string


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


contactResponseDecoder : Json.Decode.Decoder ContactsResponse
contactResponseDecoder =
    Json.Decode.Pipeline.decode ContactsResponse
        |> Json.Decode.Pipeline.required "contacts_count" Json.Decode.int
        |> Json.Decode.Pipeline.required "contacts" contactListDecoder
        |> Json.Decode.Pipeline.optional "_links"
            linksDecoder
            { next = { url = "" }, previous = { url = "" } }


linksDecoder : Json.Decode.Decoder Links
linksDecoder =
    Json.Decode.Pipeline.decode Links
        |> Json.Decode.Pipeline.optional "next"
            linkDecoder
            { url = "" }
        |> Json.Decode.Pipeline.optional "previous"
            linkDecoder
            { url = "" }


linkDecoder : Json.Decode.Decoder Link
linkDecoder =
    Json.Decode.Pipeline.decode Link
        |> Json.Decode.Pipeline.required "href" Json.Decode.string


type alias Links =
    { next : Link
    , previous : Link
    }


type alias Link =
    { url : String
    }


contactListDecoder : Json.Decode.Decoder (List Contact)
contactListDecoder =
    Json.Decode.list contactDecoder


contactDecoder : Json.Decode.Decoder Contact
contactDecoder =
    Json.Decode.Pipeline.decode Contact
        |> Json.Decode.Pipeline.required "first_name" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "last_name" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "email_address" emailDecoder


emailDecoder : Json.Decode.Decoder Email
emailDecoder =
    Json.Decode.Pipeline.decode Email
        |> Json.Decode.Pipeline.required "address" Json.Decode.string


emailListResponseDecoder : Json.Decode.Decoder EmailListResponse
emailListResponseDecoder =
    Json.Decode.Pipeline.decode EmailListResponse
        |> Json.Decode.Pipeline.required "lists" emailListListDecoder


emailListListDecoder : Json.Decode.Decoder (List EmailList)
emailListListDecoder =
    Json.Decode.list emailListDecoder


emailListDecoder : Json.Decode.Decoder EmailList
emailListDecoder =
    Json.Decode.Pipeline.decode EmailList
        |> Json.Decode.Pipeline.required "list_id" Json.Decode.string
        |> Json.Decode.Pipeline.required "name" Json.Decode.string


tagsResponseDecoder : Json.Decode.Decoder TagsResponse
tagsResponseDecoder =
    Json.Decode.Pipeline.decode TagsResponse
        |> Json.Decode.Pipeline.required "tags" tagListDecoder


tagListDecoder : Json.Decode.Decoder (List Tag)
tagListDecoder =
    Json.Decode.list tagDecoder


tagDecoder : Json.Decode.Decoder Tag
tagDecoder =
    Json.Decode.Pipeline.decode Tag
        |> Json.Decode.Pipeline.required "tag_id" Json.Decode.string
        |> Json.Decode.Pipeline.required "name" Json.Decode.string


renameModal : Model -> Html Msg
renameModal model =
    let
        currentName =
            case model.activeList of
                Nothing ->
                    ""

                Just list ->
                    list.name

        body =
            Html.form [ class "form-group" ]
                [ input
                    [ class "form-control"
                      -- FIXME - how to get default into modal
                    , placeholder currentName
                      --, value currentName
                    , onInput UpdateNewListName
                    ]
                    []
                ]
    in
        Dialog.view
            (if model.showRenameModal then
                Just
                    { closeMessage = Just AcknowledgeDialog
                    , containerClass = Just "your-container-class"
                    , header = Just (h4 [] [ text "Rename List" ])
                    , body = Just body
                    , footer =
                        Just
                            (button
                                [ class "button button-primary"
                                , onClickNoDefault CompleteListRename
                                ]
                                [ text "Rename" ]
                            )
                    }
             else
                Nothing
            )


onClickNoDefault : msg -> Attribute msg
onClickNoDefault message =
    let
        config =
            { stopPropagation = True
            , preventDefault = True
            }
    in
        onWithOptions "click" config (Json.Decode.succeed message)

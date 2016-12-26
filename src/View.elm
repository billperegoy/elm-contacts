module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Contact exposing (..)
import EmailList exposing (..)
import Tag exposing (..)
import Dialog
import Sidebar
import HttpUtils exposing (..)


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
        |> Maybe.withDefault { name = "unknown", id = "unknown" }
        |> .name


listName : String -> List EmailList -> String
listName id lists =
    lists
        |> List.filter (\list -> list.id == id)
        |> List.head
        |> Maybe.withDefault { name = "unknown", favorite = False, id = "unknown" }
        |> .name


contactsCount : Model -> Html Msg
contactsCount model =
    let
        displayText =
            case model.contactsFilterState of
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
        if model.contactsCount == 0 then
            div [] []
        else
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
            [ (Sidebar.view model.activeListMenu model.lists model.tags)
            , (mainContent model)
            , (renameModal model)
            ]
        ]


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
                    { closeMessage = Just CloseRenameModal
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

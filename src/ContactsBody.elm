module ContactsBody exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ContactsTable
import EmailList exposing (..)
import Contact exposing (..)
import Tag exposing (..)
import Model exposing (..)
import Http
import HttpUtils exposing (..)


view : Model -> Html Msg
view model =
    div [ class "col-md-9" ]
        [ (errors model.httpError)
        , (contactsCount model)
        , (ContactsTable.view model)
        , (setContactsPerPage model)
        , (pagination model)
        ]


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


errors : Maybe Http.Error -> Html Msg
errors error =
    case error of
        Nothing ->
            div [] []

        Just errorValue ->
            div [ class "alert alert-danger" ] [ text (errorString errorValue) ]


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
            a [ href "#", onClickNoDefault DisplayContactsPerPageDropdown ]
                [ text (displayString model.contactsPerPage) ]

        menu =
            if model.showContactsPerPageDropdown then
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

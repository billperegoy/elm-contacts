module ContactsTable exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Contact exposing (..)
import HttpUtils exposing (..)


view : Model -> Html Msg
view model =
    table [ class "table table-striped" ]
        [ (tableHeader model)
        , tableBody model.contacts
        ]


headerInfoRow : Model -> Html Msg
headerInfoRow model =
    let
        infoLine =
            if (List.length model.selectedContacts) == 0 then
                text
                    ((model.contactsCount |> toString)
                        ++ " Contacts. Select contacts to organize, export or remove... "
                    )
            else
                text
                    ((model.selectedContacts |> List.length |> toString) ++ " Selected")

        manageMenu =
            if (List.length model.selectedContacts) == 0 then
                span [] []
            else
                span
                    [ style
                        [ ( "margin-left"
                          , "5px"
                          )
                        ]
                    ]
                    [ a [ href "#", onClickNoDefault ShowAddToListsModal ] [ text "Add to Lists" ] ]
    in
        tr []
            [ th [ colspan 4 ]
                [ span []
                    [ infoLine
                    , manageMenu
                    ]
                ]
            ]


tableHeader : Model -> Html Msg
tableHeader model =
    let
        ids =
            List.map .id model.contacts
    in
        thead []
            [ headerInfoRow model
            , tr []
                [ th []
                    [ input
                        [ type_ "checkbox"
                          --, onCheck (SetCheckbox ids)
                        ]
                        []
                    ]
                , th [] [ text "Last Name" ]
                , th [] [ text "First Name" ]
                , th [] [ text "Email" ]
                ]
            ]


tableBody : List Contact -> Html Msg
tableBody contacts =
    tbody []
        (contactRows contacts)


contactRows : List Contact -> List (Html Msg)
contactRows contacts =
    List.map
        (\contact ->
            tr []
                [ td []
                    [ input
                        [ type_ "checkbox"
                        , onCheck (SetCheckbox contact.id)
                        ]
                        []
                    ]
                , td [] [ text (Maybe.withDefault "" contact.lastName) ]
                , td [] [ text (Maybe.withDefault "" contact.firstName) ]
                , td [] [ text contact.email.address ]
                ]
        )
        contacts

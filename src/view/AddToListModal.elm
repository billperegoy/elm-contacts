module AddToListModal exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import HttpUtils exposing (..)
import Dialog


view : Model -> Html Msg
view model =
    let
        currentName =
            case model.lists.active of
                Nothing ->
                    ""

                Just list ->
                    list.name

        buttonText =
            "Submit"

        errorPane =
            case model.httpError of
                Nothing ->
                    div [] []

                Just error ->
                    div [ class "alert alert-danger" ] [ text (errorString error) ]

        checkboxList =
            div []
                (List.map
                    (\list ->
                        p []
                            [ input
                                [ type_ "checkbox"
                                , onCheck
                                    (ProcessListCheckbox list.id)
                                ]
                                []
                            , span [ style [ ( "margin-left", "4px" ) ] ]
                                [ text list.name ]
                            ]
                    )
                    model.lists.elements
                )

        selectCount =
            div [ class "alert alert-info" ]
                [ text
                    (toString (model.lists.selected |> List.length)
                        ++ " selected"
                    )
                ]

        formAttributes =
            [ class "form-group"
            , style
                [ ( "height", "400px" )
                , ( "overflow-y", "auto" )
                ]
            ]

        body =
            div []
                [ Html.form formAttributes
                    [ errorPane
                    , checkboxList
                    ]
                , selectCount
                ]

        headerText =
            "Add " ++ (toString (model.contacts.selected |> List.length)) ++ " contacts to lists"

        modalConfig =
            { closeMessage = Just CloseAddContactsToListsModal
            , containerClass = Just "your-container-class"
            , header = Just (h4 [] [ text headerText ])
            , body = Just body
            , footer =
                Just
                    (button
                        [ class "button button-primary"
                        , onClickNoDefault SubmitAddContactsToList
                        ]
                        [ text buttonText ]
                    )
            }
    in
        Dialog.view
            (if model.lists.showAddContactsToListsModal then
                Just modalConfig
             else
                Nothing
            )

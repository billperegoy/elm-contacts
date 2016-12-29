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
            case model.activeList of
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
                    model.lists
                )

        selectCount =
            div [ class "alert alert-info" ]
                [ text
                    (toString (model.selectedLists |> List.length)
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
            "Add " ++ (toString (model.selectedContacts |> List.length)) ++ " contacts to lists"
    in
        Dialog.view
            (if model.showAddContactsToListsModal then
                Just
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
             else
                Nothing
            )

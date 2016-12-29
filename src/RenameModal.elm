module RenameModal exposing (view)

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
            if model.listHttpAction == HttpUtils.Put then
                "Rename"
            else
                "Create"

        errorPane =
            case model.httpError of
                Nothing ->
                    div [] []

                Just error ->
                    div [ class "alert alert-danger" ] [ text (errorString error) ]

        body =
            Html.form [ class "form-group" ]
                [ errorPane
                , input
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
            (if model.showListNameModal then
                Just
                    { closeMessage = Just CloseRenameModal
                    , containerClass = Just "your-container-class"
                    , header = Just (h4 [] [ text (buttonText ++ " List") ])
                    , body = Just body
                    , footer =
                        Just
                            (button
                                [ class "button button-primary"
                                , onClickNoDefault SubmitListAction
                                ]
                                [ text buttonText ]
                            )
                    }
             else
                Nothing
            )

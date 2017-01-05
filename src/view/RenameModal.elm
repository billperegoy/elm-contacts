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
            case model.lists.active of
                Nothing ->
                    ""

                Just list ->
                    list.name

        buttonText =
            if model.lists.httpAction == HttpUtils.Put then
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

        modalConfig =
            { closeMessage = Just CloseListRenameModal
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
    in
        Dialog.view
            (if model.lists.showNameModal then
                Just modalConfig
             else
                Nothing
            )

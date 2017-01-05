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
import Http
import HttpUtils exposing (..)
import AddToListModal
import RenameModal
import ContactsBody


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ div
            [ class "row" ]
            [ (Sidebar.view model.lists.displayedMenu model.lists.elements model.tags)
            , (ContactsBody.view model)
            , (RenameModal.view model)
            , (AddToListModal.view model)
            ]
        ]

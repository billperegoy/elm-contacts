module SidebarView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Contact exposing (..)
import EmailList exposing (..)
import Tag exposing (..)
import HttpUtils exposing (..)


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

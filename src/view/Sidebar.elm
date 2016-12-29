module Sidebar exposing (view)

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


listElement : Maybe String -> EmailList -> Html Msg
listElement activeMenu list =
    let
        matcher =
            Maybe.withDefault "bogus" activeMenu

        links =
            if matcher == list.id then
                ul [ style [ ( "list-style-type", "none" ) ] ]
                    [ sidebarLink (ShowRenameListModal list) "rename"
                    , sidebarLink (DeleteList list.id) "delete"
                    ]
            else
                a
                    [ class "caret"
                    , href ""
                    , onClickNoDefault (SetActiveListMenu list.id)
                    ]
                    []
    in
        li []
            [ a [ onClick (GetContacts (ByList list.id)), href "#" ] [ text list.name ]
            , links
            ]


sidebarLists : Maybe String -> List EmailList -> Html Msg
sidebarLists activeMenu lists =
    div []
        [ h4 []
            [ span [ class "label label-success" ] [ text "email lists" ]
            , a [ style [ ( "margin-left", "5px" ) ], href "#", onClickNoDefault ShowNewListModal ] [ span [ class "glyphicon glyphicon-plus" ] [] ]
            ]
        , ul [] (List.map (\list -> listElement activeMenu list) lists)
        ]


tagElement : Tag -> Html Msg
tagElement tag =
    li []
        [ a [ onClick (GetContacts (ByTag tag.id)), href "#" ] [ text tag.name ] ]


sidebarTags : List Tag -> Html Msg
sidebarTags tags =
    div []
        [ h4 [] [ span [ class "label label-success" ] [ text "tags" ] ]
        , ul [] (List.map (\tag -> tagElement tag) tags)
        ]


view : Maybe String -> List EmailList -> List Tag -> Html Msg
view activeMenu lists tags =
    div [ class "col-md-3" ]
        [ sidebarContacts
        , sidebarLists activeMenu lists
        , sidebarTags tags
        ]

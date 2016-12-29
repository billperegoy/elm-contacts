module Main exposing (..)

import Html
import Model
import Update
import View
import Subscriptions
import Contact
import TagActions


main : Program Never Model.Model Model.Msg
main =
    Html.program
        { init = Model.init ! initialActions
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }


initialActions : List (Cmd Model.Msg)
initialActions =
    [ Update.getContacts Contact.All 50
    , Update.getEmailLists
    , TagActions.getTags
    ]

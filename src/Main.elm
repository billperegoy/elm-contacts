module Main exposing (..)

import Html
import Model
import Update
import View
import Subscriptions


main : Program Never Model.Model Model.Msg
main =
    Html.program
        { init = Update.init
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }

module Main exposing (..)

import Html
import Model
import Update
import UpdateUtilities
import View
import Subscriptions


main : Program Never Model.Model Model.Msg
main =
    Html.program
        { init = UpdateUtilities.init
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }

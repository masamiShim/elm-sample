module WebApp exposing (..)

import Browser
import Html exposing (..)

-- MAIN
-- ViewがDocumentを返す様に変更
main =
    Browser.document({
        init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
    })

type alias Document msg =
    {
        title: String
        , body: List(Html msg)
    }
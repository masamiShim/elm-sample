module HttpSample exposing (..)

import Browser
import Html exposing (Html, text, pre)
import Http

main =
    Browser.element {
        init = init,
        update = update,
        subscriptions = subscriptions,
        view = view
    }

-- MODEL

type Model
    = Failure
    | Loading
    | Success String

-- モデルの初期値を返すがコマンドも発行している。
-- コマンド発行の結果をMsgとして返却
-- 初期値が関数となっているのは、JSとの相互運用で必要なため
init: () -> (Model, Cmd Msg)
init _ =
    (
        Loading,
        Http.get -- Cmd
            {
                url = "https://elm-lang.org/assets/public-opinion.txt",
                expect = Http.expectString GotText -- Httpのレスポンスを受けてからGotTextへ渡す
            }
    )

-- Resultで受けてErrorの場合のハンドリングを細かく実施する
type Msg
    = GotText ( Result Http.Error String )

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotText result ->
                case result of
                    Ok fullText ->
                        -- Cmd.noneで返しているが別途なんかして欲しいことがあればコマンドを実行する事も可能
                        (Success fullText, Cmd.none)
                    Err _ ->
                        -- Cmd.noneで返しているがリトライ等したい場合はここで頑張る
                        (Failure, Cmd.none)

-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view :Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your book"
        Loading ->
            text "Loading...."
        Success fullText ->
           pre [] [text fullText]
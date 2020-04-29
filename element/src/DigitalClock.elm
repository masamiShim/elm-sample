port module DigitalClock exposing (..)

import Browser
import Time
import Html exposing (..)
import Task

import Json.Encode as E

port cache : E.Value -> Cmd msg
port activeUsers: (E.Value -> msg) -> Sub msg
-- MAIN
main =
    Browser.element {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }

-- MODEL

type alias Model =
    {
        zone: Time.Zone,
        time: Time.Posix,
        argDate: Int
    }

init: Int -> (Model, Cmd Msg)
init time =
    ( Model Time.utc (Time.millisToPosix 0) time , Task.perform AdjustTimeZone Time.here)

-- UPDATE
type Msg
    = Tick Time.Posix | AdjustTimeZone Time.Zone

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            ({model | time = newTime}, Cmd.none)
        AdjustTimeZone newZone ->
            ({model | zone = newZone}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick

-- VIEW
view: Model -> Html Msg
view model =
    let
        hour = String.fromInt (Time.toHour model.zone model.time)
        minute = String.fromInt (Time.toMinute model.zone model.time)
        second = String.fromInt (Time.toSecond model.zone model.time)
        testTime = String.fromInt(model.argDate)
    in
        div[] [
            h1 []
            [
                text (hour ++ ":" ++ minute ++ ":" ++ second)
            ]
            , div[][text testTime]
        ]

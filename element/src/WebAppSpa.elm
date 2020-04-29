module WebAppSpa exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url

import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string)
-- ROUTING

type Route = Topic String | Blog Int | User String | Comment String Int | NotFound

routeParser: Parser (Route -> a) a
routeParser =
    oneOf
    [
        Url.Parser.map Topic (Url.Parser.s "topic" </> string)
        , Url.Parser.map Blog (Url.Parser.s "blog" </> int)
        , Url.Parser.map User (Url.Parser.s "user" </> string)
        , Url.Parser.map Comment (Url.Parser.s "user" </> string </> Url.Parser.s "comment" </> int)
    ]

urlToRoute : Url.Url -> Route
urlToRoute url =
    Maybe.withDefault NotFound <| Url.Parser.parse routeParser url

-- MAIN
main =
    Browser.application {
        init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
    }


-- MODEL

type alias Model =
    {
        key : Nav.Key
        , url : Url.Url
    }

-- initはブラウザのナビゲーションバーからURLを取得する

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
    (Model key url, Cmd.none)

-- UPDATE
type Msg = LinkClicked Browser.UrlRequest | UrlChanged Url.Url

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    ( model, Nav.load href )
        UrlChanged url ->
            ({model | url = url }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions _ = Sub.none

-- VIEW
view: Model -> Browser.Document Msg
view model =
    let
        {url} = model
    in
    case urlToRoute url of
        Topic s ->
            {
                title = "URL Interceptor"
                , body = [
                    text "The current Url is: "
                    , headerLink
                    , b[][text (Url.toString model.url)]
                    , b[][
                            text ("Topic String :" ++ s)
                        ]

                ]
            }
        Blog i ->
            {
                title = "URL Interceptor"
                , body = [
                    text "The current Url is: "
                    , headerLink
                    , b[][text (Url.toString model.url)]
                    , b[][
                        text ("Blog String :" ++ String.fromInt i)
                    ]
                ]
            }

        User s ->
            {
                title = "URL Interceptor"
                , body = [
                    text "The current Url is: "
                    , headerLink
                    , b[][text (Url.toString model.url)]
                    , b[][
                        text ("User String :" ++ s)
                    ]
                ]
            }


        Comment s i ->
            {
                title = "URL Interceptor"
                , body = [
                    text "The current Url is: "
                    , headerLink
                    , b[][text (Url.toString model.url)]
                    , b[][
                        text ("Comment String :" ++ s)
                        , text ("Comment Integer :" ++ String.fromInt i)
                        ]
                ]
            }
        NotFound ->
            {
                title = "URL Interceptor"
                , body = [
                    text "Path Not Found"
                    , headerLink
                ]
            }

viewLink: String -> Html msg
viewLink path =
    li [] [a [href path] [text path]]

headerLink:  Html msg
headerLink =
    ul[]
        [
            viewLink "/home"
            , viewLink "/topic/hoge"
            , viewLink "/topic/fuga"
            , viewLink "/topic"
            , viewLink "/user/12"
            , viewLink "/user/234"
            , viewLink "/user/aaaaa"
            , viewLink "/review/the-century-of-the-self"
            , viewLink "/review/public-opinion"
        ]
module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder)
import Models.MessageList exposing (Message, MessageList, filterByRoomId, messageDecoder)
import Models.RoomList exposing (Room, RoomList, roomDecoder)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type LoadingStatus
    = Loading
    | Done



-- ユーザ


type alias User =
    { id : Int
    , name : String
    }



-- ユーザ情報のデコーダ


userDecoder : Decoder User
userDecoder =
    Json.Decode.map2 User
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)



-- ルーム


type alias Id =
    { id : Int }


idDecoder : Decoder Id
idDecoder =
    Json.Decode.map Id
        (Json.Decode.field "id" Json.Decode.int)


fetchMessages : Cmd Msg
fetchMessages =
    Http.get
        { url = "http://localhost:3000/messages"
        , expect = Http.expectJson SetMessage messageDecoder -- Httpのレスポンスを受けてからGotTextへ渡す
        }


fetchRooms : User -> Cmd Msg
fetchRooms user =
    Http.get
        { url = "http://localhost:3000/rooms"
        , expect = Http.expectJson SetRoom roomDecoder
        }


type RequestResult
    = Success
    | Failed String


type ModalState
    = Opened
    | Closed



-- 既読


type alias Reading =
    { messageId : Int
    , userIds : List Int
    }


type alias Model =
    { loading : LoadingStatus
    , result : RequestResult
    , selected : SelectRoom
    , loginUser : User
    , messages : MessageList
    , rooms : RoomList
    , read : List Reading
    , showModal : ModalState
    , roomName : String
    }


type SelectRoom
    = Selected Int
    | UnSelectedRoom


init : Json.Decode.Value -> ( Model, Cmd Msg )
init value =
    let
        user =
            case Json.Decode.decodeValue userDecoder value of
                Ok decoded ->
                    decoded

                Err _ ->
                    User 0 "not found"
    in
    ( Model Loading Success UnSelectedRoom user (MessageList []) (RoomList []) [] Closed ""
    , Cmd.batch [ fetchRooms user, fetchMessages ]
    )


type Msg
    = SetRoom (Result Http.Error (List Room))
    | SetMessage (Result Http.Error (List Message))
    | SelectRoom Int
    | ToggleModal
    | InputRoomName String
    | AddRoom


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoom result ->
            case result of
                Ok value ->
                    ( { model | rooms = RoomList value, loading = Done, result = Success }, Cmd.none )

                Err err ->
                    ( { model | result = Failed "ルームの取得に失敗しました。", loading = Done }, Cmd.none )

        SetMessage result ->
            case result of
                Ok value ->
                    ( { model | messages = MessageList value, loading = Done, result = Success }, Cmd.none )

                Err _ ->
                    ( { model | messages = MessageList [], result = Failed "メッセージの取得に失敗しました。", loading = Done }, Cmd.none )

        SelectRoom id ->
            ( { model | selected = Selected id }, Cmd.none )

        ToggleModal ->
            case model.showModal of
                Opened ->
                    ( { model | showModal = Closed }, Cmd.none )

                Closed ->
                    ( { model | showModal = Opened }, Cmd.none )

        InputRoomName input ->
            ( { model | roomName = input }, Cmd.none )

        AddRoom ->
            ( { model | rooms = RoomList (Room (List.length model.rooms.items + 1) model.roomName [ model.loginUser.id ] :: model.rooms.items), showModal = Closed, roomName = "" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- HELPER


filterMessages : MessageList -> SelectRoom -> List Message
filterMessages messageList selected =
    case selected of
        Selected roomId ->
            case filterByRoomId messageList roomId of
                Models.MessageList.NotFound ->
                    []

                Models.MessageList.Found messages ->
                    messages

        UnSelectedRoom ->
            []



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        [ headerNav model
        , viewTitle
        , viewNote model
        , section [ class "section", class "container", class "is-fluid" ]
            [ viewContent model ]
        , viewModal model.showModal
        ]


headerNav : Model -> Html Msg
headerNav model =
    nav [ class "navbar", class "is-transparent" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", href "https://bulma.io" ]
                [ img [ src "https://bulma.io/images/bulma-logo.png", alt "Bulma: a modern CSS framework based on Flexbox", width 112, height 28 ] []
                ]
            ]
        , div [ class "navbar-menu" ]
            [ div [ class "navbar-start" ]
                [ div [ class "navbar-item" ]
                    [ p [] [ text model.loginUser.name ]
                    ]
                ]
            , div [ class "navbar-end" ]
                [ div [ class "navbar-item" ]
                    [ div [ class "buttons" ]
                        [ button [ class "button", class "is-link", onClick ToggleModal ] [ text "ルームを追加" ]
                        ]
                    ]
                ]
            ]
        ]


viewTitle : Html Msg
viewTitle =
    section [ class "hero", class "is-info" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ] [ text "Elm Chat Sample" ]
                , h2 [ class "subtitle" ] [ text "Elmでチャットのサンプル作ってみた" ]
                ]
            ]
        ]


viewNote : Model -> Html Msg
viewNote model =
    case model.result of
        Success ->
            div [] []

        Failed mes ->
            article [ class "message", class "is-danger" ]
                [ div [ class "message-header" ]
                    [ p [] [ text "メッセージ" ]
                    , button [ class "delete", ariaLabel "delete" ] []
                    ]
                , div [ class "message-body" ]
                    [ text mes
                    ]
                ]


viewContent : Model -> Html Msg
viewContent model =
    case model.loading of
        Loading ->
            div [] [ text "Loading..." ]

        Done ->
            case model.result of
                Success ->
                    div [ class "tile", class "is-ancestor" ]
                        [ div [ class "tile", class "is-vertical", class "is-4" ]
                            [ roomView model
                            ]
                        , div [ class "tile", class "is-vertical", class "is-8" ]
                            [ messageView model
                            ]
                        ]

                Failed _ ->
                    div [] [ text "読み込みに失敗しました。" ]


roomView : Model -> Html Msg
roomView model =
    div [ class "tile" ]
        [ div [ class "tile", class "is-parent", class "is-vertical" ]
            [ nav [ class "panel" ]
                [ p [ class "panel-heading" ] [ text "ルーム一覧" ]
                , div [] <|
                    List.map (\r -> panelBlock r model.selected) model.rooms.items
                ]
            ]
        ]


messageView : Model -> Html Msg
messageView model =
    let
        messagesByRoomId =
            filterMessages model.messages model.selected
    in
    if List.isEmpty messagesByRoomId then
        div [ class "tile" ]
            [ div [ class "tile", class "is-parent", class "is-vertical" ]
                [ div [ class "card" ]
                    [ header [ class "card-header", class "has-background-success" ]
                        [ p [ class "card-header-title", class "has-text-white-bis" ] [ text "メッセージ" ]
                        ]
                    , div [ class "card-content" ]
                        [ div [] [ text "メッセージはありません" ]
                        ]
                    ]
                ]
            ]

    else
        div [ class "tile" ]
            [ div [ class "tile", class "is-parent", class "is-vertical" ]
                [ div [ class "card" ]
                    [ header [ class "card-header", class "has-background-success" ]
                        [ p [ class "card-header-title", class "has-text-white-bis" ] [ text "メッセージ" ]
                        ]
                    , div [ class "card-content" ]
                        [ div [ class "content" ] <|
                            List.map (\mes -> messageContent mes (model.loginUser.id == mes.userId)) messagesByRoomId
                        ]
                    ]
                ]
            ]


panelBlock : Room -> SelectRoom -> Html Msg
panelBlock room select =
    case select of
        Selected id ->
            if id == room.id then
                a [ class "panel-block", class "is-active", class "has-background-primary", class "has-text-white-bis", onClick (SelectRoom room.id) ]
                    [ text room.name
                    ]

            else
                a [ class "panel-block", onClick (SelectRoom room.id) ]
                    [ text room.name
                    ]

        UnSelectedRoom ->
            a [ class "panel-block", onClick (SelectRoom room.id) ]
                [ text room.name
                ]


panelContent : String -> String -> Html Msg
panelContent icon iconBody =
    span [ class "panel-icon" ]
        [ i [ class icon, class iconBody, ariaHidden True ] []
        ]


messageContent : Message -> Bool -> Html Msg
messageContent message myMessage =
    if myMessage then
        article [ class "message", class "is-5", class "column" ]
            [ div [ class "message-body" ]
                [ text ("自分からのメッセージ：" ++ message.body)
                ]
            ]

    else
        article [ class "message", class "is-5", class "is-offset-7", class "column", class "is-success" ]
            [ div [ class "message-body" ]
                [ text ("相手からのメッセージ：" ++ message.body)
                ]
            ]


viewModal : ModalState -> Html Msg
viewModal modal =
    case modal of
        Opened ->
            div [ class "modal is-active" ]
                [ div [ class "modal-background", onClick ToggleModal ] []
                , div [ class "modal-card" ]
                    [ header [ class "modal-card-head" ]
                        [ p [ class "modal-card-title" ] [ text "ルームの追加" ]
                        , button [ class "delete", ariaLabel "close", onClick ToggleModal ] []
                        ]
                    , modalContent
                    , modalFooter
                    ]
                ]

        Closed ->
            div [ class "modal" ] []


modalContent : Html Msg
modalContent =
    section [ class "modal-card-body" ]
        [ div
            [ class "field" ]
            [ label [ class "label" ] [ text "ルーム名" ]
            , div [ class "control" ]
                [ input
                    [ class "input"
                    , type_ "text"
                    , placeholder "input rooom name"
                    , onInput InputRoomName
                    ]
                    []
                ]
            ]
        ]


modalFooter : Html Msg
modalFooter =
    footer [ class "modal-card-foot" ]
        [ button [ class "button", class "is-primary", onClick AddRoom ] [ text "追加する" ]
        , button [ class "button", onClick ToggleModal ] [ text "閉じる" ]
        ]

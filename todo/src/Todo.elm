module Todo exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view}

-- MODEL

type alias Todo =
    {
        title: String,
        description: String
    }

type alias Model = {todos: List Todo, title: String, description: String, filterStr: String}

init: Model
init = { todos = [], title = "", description= "", filterStr= ""}

-- UPDATE
type Msg = Add | Title String | Description String | FilterStr String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Add -> { model | todos = if invalidAdd model then model.todos else Todo model.title model.description :: model.todos}
        Title content -> {model | title = content }
        Description content -> {model | description = content}
        FilterStr content -> { model | filterStr = content}

-- VIEW
invalidAdd: Model -> Bool
invalidAdd model =
    String.isEmpty model.title || String.isEmpty model.description

view : Model -> Html Msg
view model =
    div [style "padding" "24px"]
        [
            viewTable (if String.isEmpty model.filterStr then model.todos else model.todos
                |> List.filter(\todo -> String.contains model.filterStr todo.title ))
            , button [
            style "padding" "8px 24px",
            style "cursor" "pointer",
            style "margin-top" "18px",
            onClick Add ,
            disabled (if invalidAdd model then True else False)] [ text "追加"]
            , div[
                style "margin-top" "18px"
            ]
            [
                label [
                    style "margin-right" "16px",
                    style "margin-top" "18px"

                ][text "絞り込み: "],
                input [
                style "padding" "8px 12px",
                style "width" "50%",
                style "border-radius" "8px",
                style "border" "1px solid #adadad",
                value model.filterStr
                , onInput FilterStr][]
            ]
            , div[
                    style "margin-top" "18px"
                ]
                [
                    label [
                        style "margin-right" "16px"
                ][text "タイトル: "],
                input [
                style "padding" "8px 12px",
                style "width" "50%",
                style "border-radius" "8px",
                style "border" (if emptyTitle model then "1px solid red" else "1px solid green"),
                placeholder "タイトルを入力してください",
                placeholder "タイトルを入力してください",
                value model.title,
                onInput Title][]
                , titleValidation model
            ]
            , div[
                    style "margin-top" "18px"
                ]
            [
                label [
                    style "margin-right" "16px"
                ][text "内容: "],
                input [
                style "padding" "8px 12px",
                style "width" "50%",
                style "border-radius" "8px",
                style "border" (if emptyDescription model then "1px solid red" else "1px solid green"),
                placeholder "内容を入力してください",
                value model.description, onInput Description][]
                , descriptionValidation model
            ]
            , viewValidation model
        ]

emptyTitle: Model -> Bool
emptyTitle model =
    String.isEmpty model.title

emptyDescription: Model -> Bool
emptyDescription model =
    String.isEmpty model.description
viewValidation: Model -> Html msg
viewValidation model =
    if  emptyTitle model || emptyDescription model then
        div [][]
    else
        div [style "color" "green"] [text "追加できます"]


titleValidation: Model -> Html msg
titleValidation model =
     if String.isEmpty model.title then
        div [style "color" "red"] [text "タイトルを入力してください。"]
    else
        div [style "color" "green"] [text "OK"]


descriptionValidation: Model -> Html msg
descriptionValidation model =
     if String.isEmpty model.description then
        div [style "color" "red"] [text "内容を入力してください"]
    else
        div [style "color" "green"] [text "OK"]


viewTable: List Todo -> Html Msg
viewTable todos =
    table [
        style "width" "50%"
        ]
        [ thead [
                style "border" "1px solid black" ,
                style "padding-bottom" "8px"
            ]
            [ th[style "border-bottom" "1px solid black"] [text "タイトル"]
                , th[style "border-bottom" "1px solid black"] [text "内容"]
            ]
          , tbody[] (List.map viewTr todos )
        ]

viewTr: Todo -> Html Msg
viewTr todo =
    tr[]
        [ td [] [text todo.title]
        , td [] [text todo.description]
        ]
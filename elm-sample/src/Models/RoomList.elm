module Models.RoomList exposing (..)

import Json.Decode exposing (Decoder)



-- MODEL


type ExistsRoom
    = Found Room
    | NotFound


type alias Room =
    { id : Int
    , name : String
    , users : List Int
    }


type alias RoomList =
    { items : List Room
    }



-- DECODER


roomDecoder : Decoder (List Room)
roomDecoder =
    Json.Decode.map3 Room
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "users" (Json.Decode.list Json.Decode.int))
        |> Json.Decode.list



-- HELPER


findRoomById : RoomList -> Int -> ExistsRoom
findRoomById roomList id =
    let
        found =
            roomList.items
                |> List.filter (\r -> r.id == id)
                |> List.head
    in
    case found of
        Just a ->
            Found a

        Nothing ->
            NotFound

module Models.MessageList exposing (..)

import Json.Decode exposing (Decoder)


type Found
    = NotFound
    | Found (List Message)


type alias Message =
    { id : Int
    , roomId : Int
    , userId : Int
    , body : String
    }


type alias MessageList =
    { items : List Message
    }


messageDecoder : Decoder (List Message)
messageDecoder =
    Json.Decode.map4 Message
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "roomId" Json.Decode.int)
        (Json.Decode.field "userId" Json.Decode.int)
        (Json.Decode.field "body" Json.Decode.string)
        |> Json.Decode.list


filterByRoomId : MessageList -> Int -> Found
filterByRoomId messageList roomId =
    let
        found =
            messageList.items
                |> List.filter (\m -> m.roomId == roomId)
    in
    if List.isEmpty found then
        NotFound

    else
        Found found

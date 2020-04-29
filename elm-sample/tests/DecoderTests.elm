module DecoderTests exposing (..)

import Expect
import Json.Decode as D
import Json.Encode as E
import Main exposing (User, userDecoder)
import Models.MessageList exposing (messageDecoder)
import Models.RoomList exposing (roomDecoder)
import Test exposing (..)


suite : Test
suite =
    describe "Decoder Test"
        [ describe "Decoder.User"
            [ test "Decode.Userの検証" <|
                \_ ->
                    let
                        input =
                            E.object
                                [ ( "id", E.int 1 )
                                , ( "name", E.string "hogehoge" )
                                ]

                        expected =
                            User 1 "hogehoge"

                        actual =
                            D.decodeValue userDecoder input
                    in
                    case actual of
                        Ok user ->
                            Expect.equal expected user

                        Err e1 ->
                            Debug.log ("DecodeError: " ++ D.errorToString e1)
                                Expect.fail
                                (D.errorToString e1)
            ]
        , describe "Decoder.Rooms"
            [ test "Decode.Roomの検証" <|
                \_ ->
                    let
                        input =
                            [ ( "id", E.int 1 )
                            , ( "name", E.string "hogehoge" )
                            , ( "users", E.list E.int [ 1, 2 ] )
                            ]

                        input2 =
                            [ ( "id", E.int 2 )
                            , ( "name", E.string "hogehoge2" )
                            , ( "users", E.list E.int [ 1, 2 ] )
                            ]

                        inputRoom =
                            E.list E.object [ input ]

                        expected =
                            [ Models.RoomList.Room 1 "hogehoge" [ 1, 2 ] ]

                        actual =
                            D.decodeValue roomDecoder inputRoom
                    in
                    case actual of
                        Ok room ->
                            Expect.equal expected room

                        Err e1 ->
                            Debug.log ("DecodeError: " ++ D.errorToString e1)
                                Expect.fail
                                (D.errorToString e1)
            ]
        , describe "Decoder.Message"
            [ test "Decode.Messageの検証" <|
                \_ ->
                    let
                        input =
                            [ ( "id", E.int 1 )
                            , ( "roomId", E.int 1 )
                            , ( "userId", E.int 1 )
                            , ( "body", E.string "hogehogeMessage" )
                            ]

                        inputMessages =
                            E.list E.object [ input ]

                        expected =
                            [ Models.MessageList.Message 1 1 1 "hogehogeMessage" ]

                        actual =
                            D.decodeValue messageDecoder inputMessages
                    in
                    case actual of
                        Ok message ->
                            Expect.equal expected message

                        Err e1 ->
                            Debug.log ("DecodeError: " ++ D.errorToString e1)
                                Expect.fail
                                (D.errorToString e1)
            ]
        ]

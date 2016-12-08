module Tetris exposing (..)

import Html exposing (Html, div, text, section, header)
import Html.Attributes exposing (class)
import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Array
import Maybe
import Time exposing (every, millisecond)
import Keyboard
import Random


width : Int
width =
    10


height : Int
height =
    20


blockSizeInPixels : Int
blockSizeInPixels =
    25


backgroundColor : Color
backgroundColor =
    Color.black


type Shape
    = O
    | I
    | S
    | Z
    | L
    | J
    | T


type alias Block =
    { x : Int
    , y : Int
    }


type alias Piece =
    { x : Int
    , y : Int
    , shape : Shape
    , color : Color
    , rotation : Int
    }


type alias Board =
    Array.Array Color


type alias Model =
    { board : Board
    , piece : Maybe Piece
    , nextPiece : Maybe Piece
    , gameOver : Bool
    }


type Msg
    = ClockWise
    | CounterClockWise
    | Fall
    | Left
    | Right
    | IgnoredKey
    | NextShape ( Shape, Color )


init : ( Model, Cmd Msg )
init =
    ( { board = Array.repeat (width * height) backgroundColor
      , piece = Nothing
      , nextPiece = Nothing
      , gameOver = False
      }
    , let
        nextShape =
            Random.generate NextShape <| Random.pair shapeGenerator colorGenerator
      in
        Cmd.batch [ nextShape, nextShape ]
    )


keyMap : Int -> Msg
keyMap keycode =
    case keycode of
        32 ->
            Fall

        37 ->
            Left

        38 ->
            ClockWise

        39 ->
            Right

        40 ->
            CounterClockWise

        default ->
            IgnoredKey


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.gameOver then
            Sub.none
          else
            every (millisecond * 250) <| always Fall
        , Keyboard.downs keyMap
        ]


shift : Int -> Model -> Model
shift xdir model =
    case model.piece of
        Nothing ->
            model

        Just piece ->
            let
                movedPiece =
                    { piece | x = piece.x + xdir }
            in
                if pieceCanMove model.board movedPiece then
                    { model | piece = Just movedPiece }
                else
                    model


rotate : Int -> Model -> Model
rotate rdir model =
    case model.piece of
        Nothing ->
            model

        Just piece ->
            let
                movedPiece =
                    { piece | rotation = piece.rotation + rdir }
            in
                if pieceCanMove model.board movedPiece then
                    { model | piece = Just movedPiece }
                else
                    model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClockWise ->
            ( rotate 1 model, Cmd.none )

        CounterClockWise ->
            ( rotate -1 model, Cmd.none )

        Fall ->
            case model.piece of
                Nothing ->
                    ( model, Cmd.none )

                Just piece ->
                    let
                        movedPiece =
                            { piece | y = piece.y - 1 }
                    in
                        if pieceCanMove model.board movedPiece then
                            ( { model | piece = Just movedPiece }, Cmd.none )
                        else
                            ( { model
                                | piece = Nothing
                                , board =
                                    eliminateCompleteRows <|
                                        setBoardColors
                                            (List.map
                                                (\( x, y ) ->
                                                    ( x, y, piece.color )
                                                )
                                                (pieceCoords piece)
                                            )
                                            model.board
                              }
                            , Random.generate NextShape <| Random.pair shapeGenerator colorGenerator
                            )

        Left ->
            ( shift -1 model, Cmd.none )

        Right ->
            ( shift 1 model, Cmd.none )

        IgnoredKey ->
            ( model, Cmd.none )

        NextShape ( newShape, newColor ) ->
            ( { model
                | piece = model.nextPiece
                , nextPiece = newPiece newShape newColor
                , gameOver = gameOver { model | piece = model.nextPiece }
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "center" ]
        [ section [ class "game" ]
            [ header [ class "title" ] [ Html.text "TETRIS" ]
            , div [ class "group" ]
                [ div [ class "arena" ]
                    [ div [ class "board" ]
                        [ Element.toHtml <|
                            collage (width * blockSizeInPixels) (height * blockSizeInPixels) <|
                                List.append
                                    (viewBlocks model.board)
                                <|
                                    case model.piece of
                                        Nothing ->
                                            []

                                        Just piece ->
                                            (viewPiece ( width, height ) piece)
                        ]
                    , div [ class "next" ]
                        [ case model.nextPiece of
                            Nothing ->
                                Html.text ""

                            Just piece ->
                                let
                                    size =
                                        4
                                in
                                    Element.toHtml <|
                                        collage (size * blockSizeInPixels) (size * blockSizeInPixels) <|
                                            List.append [] (viewPiece ( size, size ) { piece | x = (size // 2) - 1, y = size - 1 })
                        ]
                    ]
                ]
            ]
        ]


eliminateCompleteRows : Board -> Board
eliminateCompleteRows board =
    let
        -- Get the indices of the rows that are filled from larges to smallest
        completedRows =
            List.reverse <|
                List.filter (\y -> rowFilled y board) <|
                    List.range 0 height
    in
        List.foldl (\y board -> dropRows y board) board completedRows


shapeGenerator : Random.Generator Shape
shapeGenerator =
    Random.map
        (\n ->
            case n of
                1 ->
                    O

                2 ->
                    I

                3 ->
                    S

                4 ->
                    Z

                5 ->
                    L

                6 ->
                    J

                default ->
                    T
        )
        (Random.int 1 7)


colorGenerator : Random.Generator Color
colorGenerator =
    Random.map
        (\shape ->
            case shape of
                1 ->
                    Color.red

                2 ->
                    Color.green

                3 ->
                    Color.yellow

                4 ->
                    Color.orange

                5 ->
                    Color.blue

                default ->
                    Color.black
        )
        (Random.int 1 5)


pieceCanMove : Board -> Piece -> Bool
pieceCanMove board piece =
    List.foldl
        (\( x_, y_ ) last ->
            last
                && not
                    (boardFilled board ( x_, y_ )
                        || y_
                        < 0
                        || x_
                        < 0
                        || x_
                        >= width
                    )
        )
        True
    <|
        pieceCoords piece


arrayGetWithDefault : d -> Int -> Array.Array d -> d
arrayGetWithDefault default index array =
    Maybe.withDefault default <| Array.get index array


transform : ( Int, Int ) -> ( Int, Int ) -> ( Float, Float )
transform ( x, y ) ( w, h ) =
    let
        pixelRootX =
            toFloat <|
                -((w * blockSizeInPixels) - blockSizeInPixels)
                    // 2

        pixelX =
            toFloat <| blockSizeInPixels * x

        pixelRootY =
            toFloat <|
                -((h * blockSizeInPixels) - blockSizeInPixels)
                    // 2

        pixelY =
            toFloat <| blockSizeInPixels * y
    in
        ( pixelRootX + pixelX, pixelRootY + pixelY )


newPiece : Shape -> Color -> Maybe Piece
newPiece shape color =
    Just { x = (width // 2) - 1, y = height - 1, shape = shape, color = color, rotation = 0 }


boardIndex : ( Int, Int ) -> Int
boardIndex ( x, y ) =
    x + (width * (height - (y + 1)))


setBoardColors : List ( Int, Int, Color ) -> Board -> Board
setBoardColors coords board =
    case coords of
        [] ->
            board

        ( x, y, c ) :: cs ->
            setBoardColors cs (Array.set (boardIndex ( x, y )) c board)


boardColor : Board -> ( Int, Int ) -> Color
boardColor board coord =
    arrayGetWithDefault backgroundColor (boardIndex coord) board


gameOver : Model -> Bool
gameOver model =
    case model.piece of
        Nothing ->
            False

        Just piece ->
            not <| pieceCanMove model.board piece


rowFilled : Int -> Board -> Bool
rowFilled y board =
    List.foldl (\x stat -> stat && (boardFilled board ( x, y ))) True <| List.range 0 (width - 1)


dropRows : Int -> Board -> Board
dropRows y board =
    List.foldl (\y board -> dropRow y board) board <| List.range y (height - 1)


dropRow : Int -> Board -> Board
dropRow y board =
    let
        coords =
            if y == height - 1 then
                List.map (\x -> ( x, y, backgroundColor )) <| List.range 0 width
            else
                List.map (\x -> ( x, y, boardColor board ( x, (y + 1) ) )) <| List.range 0 width
    in
        setBoardColors coords board


boardFilled : Board -> ( Int, Int ) -> Bool
boardFilled board coord =
    boardColor board coord /= backgroundColor


pieceCoords : Piece -> List ( Int, Int )
pieceCoords piece =
    let
        pieces =
            (Array.fromList
                (case piece.shape of
                    O ->
                        [ [ ( 0, -1 ), ( 1, -1 ), ( 0, 0 ), ( 1, 0 ) ] ]

                    I ->
                        [ [ ( 0, 0 ), ( -1, 0 ), ( 1, 0 ), ( 2, 0 ) ]
                        , [ ( 0, 0 ), ( 0, -1 ), ( 0, -2 ), ( 0, 1 ) ]
                        ]

                    S ->
                        [ [ ( 0, 0 ), ( 1, 0 ), ( 0, -1 ), ( -1, -1 ) ]
                        , [ ( 0, 0 ), ( -1, 1 ), ( -1, 0 ), ( 0, -1 ) ]
                        ]

                    Z ->
                        [ [ ( 0, 0 ), ( -1, 0 ), ( 0, -1 ), ( 1, -1 ) ]
                        , [ ( 0, 0 ), ( 1, 1 ), ( 1, 0 ), ( 0, -1 ) ]
                        ]

                    L ->
                        [ [ ( 0, 0 ), ( 1, 0 ), ( -1, 0 ), ( -1, -1 ) ]
                        , [ ( 0, 0 ), ( 0, -1 ), ( 0, 1 ), ( -1, 1 ) ]
                        , [ ( 0, -1 ), ( -1, -1 ), ( 1, -1 ), ( 1, 0 ) ]
                        , [ ( 0, 0 ), ( 0, 1 ), ( 0, -1 ), ( 1, -1 ) ]
                        ]

                    J ->
                        [ [ ( 0, 0 ), ( 1, 0 ), ( -1, 0 ), ( 1, -1 ) ]
                        , [ ( 0, 0 ), ( 0, -1 ), ( 0, 1 ), ( -1, -1 ) ]
                        , [ ( 0, -1 ), ( -1, -1 ), ( 1, -1 ), ( -1, 0 ) ]
                        , [ ( 0, 0 ), ( 0, 1 ), ( 0, -1 ), ( 1, 1 ) ]
                        ]

                    T ->
                        [ [ ( 0, 0 ), ( -1, 0 ), ( 1, 0 ), ( 0, -1 ) ]
                        , [ ( 0, 0 ), ( 0, -1 ), ( 0, 1 ), ( -1, 0 ) ]
                        , [ ( 0, -1 ), ( -1, -1 ), ( 1, -1 ), ( 0, 0 ) ]
                        , [ ( 0, 0 ), ( 0, -1 ), ( 0, 1 ), ( 1, 0 ) ]
                        ]
                )
            )
    in
        List.map
            (\( x, y ) -> ( piece.x + x, piece.y + y ))
            (case Array.get (piece.rotation % Array.length pieces) pieces of
                Nothing ->
                    []

                Just a ->
                    a
            )


viewPiece : ( Int, Int ) -> Piece -> List Form
viewPiece ( w, h ) piece =
    List.map
        (\( x, y ) ->
            Collage.move (transform ( x, y ) ( w, h )) <|
                filled piece.color <|
                    Collage.square <|
                        toFloat blockSizeInPixels
        )
        (pieceCoords piece)


viewBlock : Board -> ( Int, Int ) -> Form
viewBlock board coord =
    Collage.move (transform coord ( width, height )) <|
        filled (boardColor board coord) <|
            Collage.square <|
                toFloat blockSizeInPixels


cartesianProduct2 : List a -> List b -> List ( a, b )
cartesianProduct2 listA listB =
    List.concat <|
        List.map (\a -> List.map (\b -> ( a, b )) <| listB) <|
            listA


viewBlocks : Board -> List Form
viewBlocks board =
    List.map
        (\coord -> viewBlock board coord)
        (cartesianProduct2 (List.range 0 width) (List.range 0 height))

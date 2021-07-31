module Main exposing (..)

import Array exposing (Array, indexedMap)
import Browser
import Browser.Events exposing (onKeyDown)
import Dict as Dict exposing (Dict)
import Html exposing (Html, button, div, img, p, text)
import Html.Attributes exposing (id, src, style)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Process exposing (sleep)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { mode : Mode
    }


init : ( Model, Cmd Msg )
init =
    let
        firstStatge =
            Maybe.withDefault { matrixSize = 0, initialCellPlaces = [], objectPlacement = [] } <| Dict.get 1 stages

        initConfig =
            { status = Play
            , stageNumber = 1
            , matrixSize = firstStatge.matrixSize
            , board = initBoard firstStatge
            , objectPlacement = Dict.fromList firstStatge.objectPlacement
            }
    in
    ( { mode = Normal initConfig
      }
    , Cmd.none
    )


type alias Board =
    Array Cell


type Cell
    = Floor
    | Wall
    | Container
    | Cord
    | Me


type Mode
    = Normal GameConfig


type alias GameConfig =
    { status : GameStatus
    , stageNumber : Int
    , matrixSize : Int
    , board : Board
    , objectPlacement : Dict Int Cell
    }


type GameStatus
    = Play
    | Clear
    | Compleate


type alias Stage =
    { matrixSize : Int
    , initialCellPlaces : List ( Int, Cell )
    , objectPlacement : List ( Int, Cell )
    }


createStage : String -> List ( Int, Cell )
createStage str =
    String.replace "\n" "" str
        |> String.toList
        |> List.indexedMap Tuple.pair
        |> List.filter (\( i, c ) -> c /= ' ')
        |> List.map
            (\( i, c ) ->
                ( i
                , if c == 'w' then
                    Wall

                  else if c == 'c' then
                    Container

                  else if c == 'm' then
                    Me

                  else if c == '.' then
                    Cord

                  else
                    Floor
                )
            )


stage1 : String
stage1 =
    """
w  www
wc m  
w c w 
  w c 
...   
wwwwww
"""


stage2 : String
stage2 =
    """
wwwwwwwwwwww
wwwwwwwwwwww
wwwwwwwwwwww
wwwwwwwwwwww
w     ww m w
    c .. c w
wwwwwwwwwwww
wwwwwwwwwwww
wwwwwwwwwwww
wwwwwwwwwwww
wwwwwwwwwwww
wwwwwwwwwwww
"""


stages : Dict Int Stage
stages =
    let
        initial : String -> List ( Int, Cell )
        initial stage =
            List.filter (\( _, cell ) -> cell == Me || cell == Container) (createStage stage)

        base : String -> List ( Int, Cell )
        base stage =
            List.filter
                (\( _, cell ) -> cell == Wall || cell == Cord)
                (createStage stage)
    in
    Dict.fromList
        [ ( 1
          , Stage 6 (initial stage1) (base stage1)
          )
        , ( 2
          , Stage 12 (initial stage2) (base stage2)
          )
        ]


initBoard : Stage -> Array Cell
initBoard stage =
    Array.initialize
        (stage.matrixSize * stage.matrixSize)
        (\n ->
            Maybe.withDefault Floor <| Dict.get n <| Dict.fromList <| stage.objectPlacement ++ stage.initialCellPlaces
        )



-- UPDATE


type Msg
    = KeyDown Direction
    | ChangeNextStage
    | Reset


type Direction
    = Left
    | Up
    | Right
    | Down


getMyIndex : Board -> Int
getMyIndex board =
    case find (\cell -> Tuple.second cell == Me) (Array.toIndexedList board) of
        Just ( i, _ ) ->
            i

        Nothing ->
            -1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.mode of
        Normal gameConfig ->
            case msg of
                KeyDown direction ->
                    let
                        current =
                            getMyIndex gameConfig.board

                        nextMyPoint =
                            move { direction = direction, matrixSize = gameConfig.matrixSize, current = current } gameConfig.board Me

                        nextCellMaybe =
                            Array.get nextMyPoint gameConfig.board

                        nextContainerPoint =
                            move { direction = direction, matrixSize = gameConfig.matrixSize, current = nextMyPoint } gameConfig.board Container

                        placedObject =
                            Maybe.withDefault Floor <| Dict.get current gameConfig.objectPlacement

                        newBoard =
                            setCell { cell = Me, point = nextMyPoint }
                                << setCell { cell = placedObject, point = current }

                        board =
                            if nextCellMaybe == Just Container && nextContainerPoint == nextMyPoint then
                                gameConfig.board

                            else if nextCellMaybe == Just Container then
                                (newBoard
                                    << setCell
                                        { cell = Container
                                        , point = nextContainerPoint
                                        }
                                )
                                    gameConfig.board

                            else
                                newBoard gameConfig.board

                        gameStatus =
                            judgeGameStatus gameConfig.objectPlacement board
                    in
                    case gameConfig.status of
                        Clear ->
                            ( model, Cmd.none )

                        Compleate ->
                            ( model, Cmd.none )

                        _ ->
                            case gameStatus of
                                Play ->
                                    ( { model
                                        | mode =
                                            Normal
                                                { gameConfig
                                                    | status = gameConfig.status
                                                    , board = board
                                                }
                                      }
                                    , Cmd.none
                                    )

                                Clear ->
                                    ( { model | mode = Normal { gameConfig | status = Clear, board = board } }, Task.perform (always ChangeNextStage) <| Process.sleep 2000 )

                                _ ->
                                    ( model, Cmd.none )

                ChangeNextStage ->
                    let
                        nextStageNumber =
                            gameConfig.stageNumber + 1

                        nextStage =
                            Maybe.withDefault { matrixSize = 0, initialCellPlaces = [], objectPlacement = [] } <| Dict.get nextStageNumber stages
                    in
                    if nextStageNumber <= Dict.size stages then
                        ( { mode =
                                Normal
                                    { gameConfig
                                        | status = Play
                                        , stageNumber = nextStageNumber
                                        , matrixSize = nextStage.matrixSize
                                        , board = initBoard nextStage
                                        , objectPlacement = Dict.fromList nextStage.objectPlacement
                                    }
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | mode = Normal { gameConfig | status = Compleate } }, Cmd.none )

                Reset ->
                    let
                        stage =
                            Maybe.withDefault { matrixSize = 0, initialCellPlaces = [], objectPlacement = [] } <| Dict.get gameConfig.stageNumber stages
                    in
                    ( { model | mode = Normal { gameConfig | board = initBoard stage } }, Cmd.none )


type alias MoveArg =
    { direction : Direction, matrixSize : Int, current : Int }


move : MoveArg -> Board -> Cell -> Int
move { direction, matrixSize, current } board cell =
    let
        destination : Int
        destination =
            moveHelper { direction = direction, matrixSize = matrixSize, current = current }
    in
    case Array.get destination board of
        Just Wall ->
            current

        Just Container ->
            case cell of
                Container ->
                    current

                _ ->
                    destination

        _ ->
            destination


moveHelper : MoveArg -> Int
moveHelper { direction, matrixSize, current } =
    current
        + (if
            (modBy matrixSize current == 0 && direction == Left)
                || (current < matrixSize && direction == Up)
                || (modBy matrixSize current == matrixSize - 1 && direction == Right)
                || ((current + matrixSize) >= (matrixSize * matrixSize) && direction == Down)
           then
            0

           else
            case direction of
                Left ->
                    -1

                Up ->
                    -matrixSize

                Right ->
                    1

                Down ->
                    matrixSize
          )


setCell : { cell : Cell, point : Int } -> Board -> Board
setCell { cell, point } =
    Array.set point cell


judgeGameStatus : Dict Int Cell -> Board -> GameStatus
judgeGameStatus objectPlacement board =
    let
        baseCordIndexList =
            List.sort <|
                List.map Tuple.first <|
                    Dict.toList <|
                        Dict.filter (\_ cell -> cell == Cord) objectPlacement

        containerIndexList =
            List.sort <|
                List.map Tuple.first <|
                    List.filter (\( _, cell ) -> cell == Container) <|
                        Array.toIndexedList board
    in
    if baseCordIndexList == containerIndexList then
        Clear

    else
        Play



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown <| JD.map (\d -> KeyDown d) keyDecoder


keyDecoder : JD.Decoder Direction
keyDecoder =
    JD.andThen toDirection (JD.field "key" JD.string)


toDirection : String -> JD.Decoder Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            JD.succeed Left

        "ArrowUp" ->
            JD.succeed Up

        "ArrowRight" ->
            JD.succeed Right

        "ArrowDown" ->
            JD.succeed Down

        _ ->
            JD.fail "can use direction key only"



-- VIEW


view : Model -> Html Msg
view model =
    let
        attrs =
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "margin" "1vmin 0"
            ]
    in
    case model.mode of
        Normal gameConfig ->
            div [ style "margin-top" <| vmin 5.0 ]
                [ div
                    attrs
                    [ button
                        [ onClick Reset
                        , style "height" <| vmin 5.0
                        ]
                        [ text "このステージをリセット" ]
                    ]
                , div attrs
                    [ viewBoard gameConfig
                    , if gameConfig.status == Clear || gameConfig.status == Compleate then
                        viewGameStatus gameConfig

                      else
                        text ""
                    ]
                ]


viewBoard : GameConfig -> Html msg
viewBoard gameConfig =
    let
        sideLength =
            vmin boardLength

        cellSideLength =
            boardLength / toFloat gameConfig.matrixSize

        attrs =
            [ style "width" sideLength
            , style "height" sideLength
            ]
    in
    div attrs
        (List.map (\cell -> viewCell cellSideLength cell) (Array.toList gameConfig.board))


viewCell : Float -> Cell -> Html msg
viewCell sideLength cell =
    let
        styles =
            [ style "width" <| vmin sideLength
            , style "height" <| vmin sideLength
            , style "margin" "0"
            , style "display" "block"
            , style "float" "left"
            , style "border" "solid 0.1vmin #ccc"
            , style "box-sizing" "border-box"
            ]
    in
    img (src (floor cell) :: styles) []


viewGameStatus : GameConfig -> Html msg
viewGameStatus gameConfig =
    let
        attrs =
            [ id
                (if gameConfig.status == Compleate then
                    "fullOverlay_complete"

                 else
                    "fullOverlay"
                )
            ]

        result =
            case gameConfig.status of
                Clear ->
                    "ステージクリア!"

                Compleate ->
                    "全ステージクリア!"

                _ ->
                    ""
    in
    div attrs
        [ p
            [ style "text-align" "center"
            , style "font-size" "60px"
            , style "font-weight" "bold"
            ]
            [ text result ]
        ]


floor : Cell -> String
floor cell =
    case cell of
        Floor ->
            "http://drive.google.com/uc?export=view&id=1jd773St3MIlhMxNcDPJCMq9aUgMrdF0i"

        Wall ->
            "http://drive.google.com/uc?export=view&id=1pXYU3PelaYMUbD2m06noTxXoCkwnV9yP"

        Container ->
            "http://drive.google.com/uc?export=view&id=1J2wvdXZQ6f1bK8tBqPqLa8tszV5HSTAb"

        Cord ->
            "http://drive.google.com/uc?export=view&id=11hmjC2DshMm2DsjcVbPXsXTI9K36sUUG"

        Me ->
            "http://drive.google.com/uc?export=view&id=1I_FyCkRwUVTi9we0g_cMI5R3KY9s-N72"


boardLength : Float
boardLength =
    80.0


vmin : Float -> String
vmin n =
    String.append (String.fromFloat n) "vmin"


find : (a -> Bool) -> List a -> Maybe a
find f list =
    List.head <| List.filter f list

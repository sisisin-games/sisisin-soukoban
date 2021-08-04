module Main exposing (..)

import Array exposing (Array, indexedMap)
import Browser
import Browser.Events exposing (onKeyDown)
import Dict as Dict exposing (Dict)
import Html exposing (Html, br, button, div, img, input, li, p, text, textarea, ul)
import Html.Attributes exposing (id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
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
    , gameConfig : GameConfig
    }


initConfig =
    let
        firstStatge =
            Maybe.withDefault { matrixSize = 0, initialCellPlaces = [], objectPlacement = [] } <| Dict.get 1 stages
    in
    { status = Play
    , stageNumber = 1
    , matrixSize = firstStatge.matrixSize
    , board = initBoard firstStatge
    , objectPlacement = Dict.fromList firstStatge.objectPlacement
    }


init : ( Model, Cmd Msg )
init =
    ( { mode = Select NormalPlay
      , gameConfig =
            { status = Play
            , stageNumber = 0
            , matrixSize = 0
            , board = Array.empty
            , objectPlacement = Dict.empty
            }
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
    = Select Option
    | InputOriginalStage OriginalStageConfig
    | Normal GameConfig


type Option
    = NormalPlay
    | OriginalPlay


type alias OriginalStageConfig =
    { matrixSize : String
    , board : String
    }


initOriginalStageConfig =
    { matrixSize = "3"
    , board = ""
    }


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
    Dict.fromList
        [ ( 1
          , Stage 6 (initial stage1) (base stage1)
          )
        , ( 2
          , Stage 12 (initial stage2) (base stage2)
          )
        ]


initial : String -> List ( Int, Cell )
initial stage =
    List.filter (\( _, cell ) -> cell == Me || cell == Container) (createStage stage)


base : String -> List ( Int, Cell )
base stage =
    List.filter
        (\( _, cell ) -> cell == Wall || cell == Cord)
        (createStage stage)


initBoard : Stage -> Array Cell
initBoard stage =
    Array.initialize
        (stage.matrixSize * stage.matrixSize)
        (\n ->
            Maybe.withDefault Floor <| Dict.get n <| Dict.fromList <| stage.objectPlacement ++ stage.initialCellPlaces
        )



-- UPDATE


type Msg
    = KeyDown Key
    | ChangeNextStage
    | Reset
    | GenerateStage
    | ChangeMaxtrixSizeInput String
    | ChangeBoardInput String


type Key
    = Left
    | Up
    | Right
    | Down
    | Enter


getMyIndex : Board -> Int
getMyIndex board =
    case find (\cell -> Tuple.second cell == Me) (Array.toIndexedList board) of
        Just ( i, _ ) ->
            i

        Nothing ->
            -1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            ( model, Cmd.none )

        ChangeNextStage ->
            ( model, Cmd.none )

        Reset ->
            let
                stage =
                    Maybe.withDefault { matrixSize = 0, initialCellPlaces = [], objectPlacement = [] } <| Dict.get model.gameConfig.stageNumber stages

                gameConfig =
                    model.gameConfig

                updatedConfig =
                    { gameConfig | board = initBoard stage }
            in
            ( { model | gameConfig = updatedConfig }, Cmd.none )

        GenerateStage ->
            ( model, Cmd.none )

        ChangeMaxtrixSizeInput input ->
            ( model, Cmd.none )

        ChangeBoardInput input ->
            ( model, Cmd.none )



-- case model.mode of
--     Normal gameConfig ->
--         case msg of
--             KeyDown key ->
--                 let
--                     current =
--                         getMyIndex gameConfig.board
--                     nextMyPoint =
--                         move { key = key, matrixSize = gameConfig.matrixSize, current = current } gameConfig.board Me
--                     nextCellMaybe =
--                         Array.get nextMyPoint gameConfig.board
--                     nextContainerPoint =
--                         move { key = key, matrixSize = gameConfig.matrixSize, current = nextMyPoint } gameConfig.board Container
--                     placedObject =
--                         Maybe.withDefault Floor <| Dict.get current gameConfig.objectPlacement
--                     newBoard =
--                         setCell { cell = Me, point = nextMyPoint }
--                             << setCell { cell = placedObject, point = current }
--                     board =
--                         if nextCellMaybe == Just Container && nextContainerPoint == nextMyPoint then
--                             gameConfig.board
--                         else if nextCellMaybe == Just Container then
--                             (newBoard
--                                 << setCell
--                                     { cell = Container
--                                     , point = nextContainerPoint
--                                     }
--                             )
--                                 gameConfig.board
--                         else
--                             newBoard gameConfig.board
--                     gameStatus =
--                         judgeGameStatus gameConfig.objectPlacement board
--                 in
--                 case gameConfig.status of
--                     Clear ->
--                         ( model, Cmd.none )
--                     Compleate ->
--                         ( model, Cmd.none )
--                     _ ->
--                         case gameStatus of
--                             Play ->
--                                 ( { model
--                                     | mode =
--                                         Normal
--                                             { gameConfig
--                                                 | status = gameConfig.status
--                                                 , board = board
--                                             }
--                                   }
--                                 , Cmd.none
--                                 )
--                             Clear ->
--                                 ( { model | mode = Normal { gameConfig | status = Clear, board = board } }, Task.perform (always ChangeNextStage) <| Process.sleep 2000 )
--                             _ ->
--                                 ( model, Cmd.none )
--             ChangeNextStage ->
--                 let
--                     nextStageNumber =
--                         gameConfig.stageNumber + 1
--                     nextStage =
--                         Maybe.withDefault { matrixSize = 0, initialCellPlaces = [], objectPlacement = [] } <| Dict.get nextStageNumber stages
--                 in
--                 if gameConfig.stageNumber == -1 then
--                     ( { model | mode = Normal { gameConfig | status = Compleate } }, Cmd.none )
--                 else if nextStageNumber <= Dict.size stages then
--                     ( { mode =
--                             Normal
--                                 { gameConfig
--                                     | status = Play
--                                     , stageNumber = nextStageNumber
--                                     , matrixSize = nextStage.matrixSize
--                                     , board = initBoard nextStage
--                                     , objectPlacement = Dict.fromList nextStage.objectPlacement
--                                 }
--                       }
--                     , Cmd.none
--                     )
--                 else
--                     ( { model | mode = Normal { gameConfig | status = Compleate } }, Cmd.none )
--             Reset ->
--                 let
--                     stage =
--                         Maybe.withDefault { matrixSize = 0, initialCellPlaces = [], objectPlacement = [] } <| Dict.get gameConfig.stageNumber stages
--                 in
--                 ( { model | mode = Normal { gameConfig | board = initBoard stage } }, Cmd.none )
--             _ ->
--                 ( model, Cmd.none )
--     Select option ->
--         case msg of
--             KeyDown Enter ->
--                 case option of
--                     NormalPlay ->
--                         ( { model | mode = Normal initConfig }, Cmd.none )
--                     OriginalPlay ->
--                         ( { model | mode = InputOriginalStage initOriginalStageConfig }, Cmd.none )
--             KeyDown Up ->
--                 ( { model
--                     | mode =
--                         Select
--                             (if option == NormalPlay then
--                                 OriginalPlay
--                              else
--                                 NormalPlay
--                             )
--                   }
--                 , Cmd.none
--                 )
--             KeyDown Down ->
--                 ( { model
--                     | mode =
--                         Select
--                             (if option == NormalPlay then
--                                 OriginalPlay
--                              else
--                                 NormalPlay
--                             )
--                   }
--                 , Cmd.none
--                 )
--             _ ->
--                 ( model, Cmd.none )
--     InputOriginalStage config ->
--         case msg of
--             ChangeMaxtrixSizeInput input ->
--                 ( { model | mode = InputOriginalStage { config | matrixSize = input } }, Cmd.none )
--             ChangeBoardInput input ->
--                 ( { model | mode = InputOriginalStage { config | board = input } }, Cmd.none )
--             GenerateStage ->
--                 let
--                     --TODO: 値が正しいかバリデーションする
--                     matrixSize =
--                         Maybe.withDefault 0 <| String.toInt config.matrixSize
--                     stage : Stage
--                     stage =
--                         Stage matrixSize (initial config.board) (base config.board)
--                     gameConfig =
--                         { status = Play
--                         , stageNumber = -1
--                         , matrixSize = matrixSize
--                         , board = initBoard stage
--                         , objectPlacement = Dict.fromList stage.objectPlacement
--                         }
--                 in
--                 ( { model | mode = Normal gameConfig }, Cmd.none )
--             _ ->
--                 ( model, Cmd.none )


type alias MoveArg =
    { key : Key, matrixSize : Int, current : Int }


move : MoveArg -> Board -> Cell -> Int
move { key, matrixSize, current } board cell =
    let
        destination : Int
        destination =
            moveHelper { key = key, matrixSize = matrixSize, current = current }
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
moveHelper { key, matrixSize, current } =
    current
        + (if
            (modBy matrixSize current == 0 && key == Left)
                || (current < matrixSize && key == Up)
                || (modBy matrixSize current == matrixSize - 1 && key == Right)
                || ((current + matrixSize) >= (matrixSize * matrixSize) && key == Down)
           then
            0

           else
            case key of
                Left ->
                    -1

                Up ->
                    -matrixSize

                Right ->
                    1

                Down ->
                    matrixSize

                _ ->
                    0
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
    case model.mode of
        Normal _ ->
            onKeyDown <| JD.map (\d -> KeyDown d) (keyDecoder toDirection)

        Select _ ->
            onKeyDown <| JD.map (\d -> KeyDown d) (keyDecoder toDirection)

        _ ->
            Sub.none


keyDecoder : (String -> JD.Decoder a) -> JD.Decoder a
keyDecoder toA =
    JD.andThen toA (JD.field "key" JD.string)


toDirection : String -> JD.Decoder Key
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

        "Enter" ->
            JD.succeed Enter

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

        Select option ->
            let
                options =
                    [ ( NormalPlay, "Game Play" ), ( OriginalPlay, "Game Play on Original Stage" ) ]

                lis =
                    List.map
                        (\t ->
                            li
                                [ style "list-style-type"
                                    (if option == Tuple.first t then
                                        "square"

                                     else
                                        "none"
                                    )
                                ]
                                [ text (Tuple.second t) ]
                        )
                        options
            in
            div []
                [ img [ style "width" "100%", src logo ] []
                , div [ style "margin-top" <| vmin 5, style "margin-left" <| vmin 25 ]
                    [ text "カーソルキーでモードを選んで、Enterを押す"
                    , ul [ style "font-size" <| vmin 7 ] lis
                    ]
                ]

        InputOriginalStage config ->
            div []
                [ img [ style "width" "100%", src logo ] []
                , div [ style "margin-top" <| vmin 5, style "margin-bottom" <| vmin 5, style "margin-left" <| vmin 25 ]
                    [ p [] [ text "一辺のマス数" ]
                    , input [ value config.matrixSize, type_ "number", onInput ChangeMaxtrixSizeInput ] []
                    , p [] [ text "盤面（入力方式： ①自分m 壁w 座標. コンテナc 床 その他 or 入力なし） ②改行は入れてもよい" ]
                    , textarea [ placeholder "盤面", value config.board, onInput ChangeBoardInput ] []
                    , br [] []
                    , p [ style "background-color" "#f5f5f5", style "border-radius" "5px", style "width" "30%", style "font-size" "80%", style "padding" <| vmin 1, style "margin-bottom" <| vmin 2 ]
                        [ text "(入力例)"
                        , li [] [ text "一辺のマス数:", br [] [], text "3" ]
                        , li []
                            [ text "盤面:"
                            , br [] []
                            , text "www"
                            , br [] []
                            , text ".cm"
                            , br [] []
                            , text "w"
                            ]
                        ]
                    , button [ onClick GenerateStage ] [ text "このステージで遊ぶ" ]
                    , p [] [ text "※バリデーションをまだかけていません。意図しないステージが出来上がったらリロードして作り直してください。" ]
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


logo =
    "http://drive.google.com/uc?export=view&id=1UKHNjZ4oaDgCGZrNaaicAt45iM4nKnya"


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

module MainTest exposing (..)

import Array
import Dict as Dict exposing (Dict)
import Expect
import Main exposing (..)
import Test exposing (..)



initBoardTest : Test
initBoardTest =
    describe "initBoard test"
        [ describe "a"
            [ test "cellsの指定した場所以外はFloorになる" <|
                \_ ->
                    initBoard { matrixSize = 2, initialCellPlaces = [ ( 1, Me ) ], objectPlacement = [] }
                        |> Array.get 0
                        |> Expect.equal (Just Floor)
            ]
        ]


updateTest : Test
updateTest =
    let
        dummyConfig: GameConfig
        dummyConfig =
            {
                    stageNumber = 1
                    , status = Play
                    , matrixSize = 0
                    , board = Array.fromList [ Floor, Me ]
                    , objectPlacement = Dict.fromList []
            }
        dummyModel: Model
        dummyModel =
            {
                mode = Normal dummyConfig
            }

        doMoveBoard msg model =
            update msg model
                |> Tuple.first
                |> .board
    in
    describe "updateTest test"
        [ describe "KeyDownを受け取ったとき"
            [ test "移動先:FloorかつKeyDown:Leftの場合、Meは左方向に動く" <|
                \_ ->
                    doMoveBoard (KeyDown Left)
                        { dummyModel
                            | mode = Normal {
                                dummyConfig
                                | matrixSize = 2
                                , board = Array.fromList [ Floor, Me ]
                            }
                        }
                        |> Expect.equal (Array.fromList [ Me, Floor ])
            , test "移動先:FloorかつKeyDown:Rightの場合、Meは右方向に動く" <|
                \_ ->
                    doMoveBoard (KeyDown Right)
                        { dummyModel
                            | mode = Normal {
                                dummyConfig
                                | matrixSize = 2
                                , board = Array.fromList [ Me, Floor ]
                            }
                        }
                        |> Expect.equal (Array.fromList [ Floor, Me ])
            , test "移動先:FloorかつKeyDown:Upの場合、Meは上方向に動く" <|
                \_ ->
                    doMoveBoard (KeyDown Up)
                        { dummyModel
                            | matrixSize = 2
                            , board = Array.fromList [ Floor, Floor, Me, Floor ]
                        }
                        |> Expect.equal (Array.fromList [ Me, Floor, Floor, Floor ])
            , test "移動先:FloorかつKeyDown:Downの場合、Meは下方向に動く" <|
                \_ ->
                    doMoveBoard (KeyDown Down)
                        { dummyModel
                            | matrixSize = 2
                            , board = Array.fromList [ Me, Floor, Floor, Floor ]
                        }
                        |> Expect.equal (Array.fromList [ Floor, Floor, Me, Floor ])
            , test "移動先:ContainerでContainerも移動可能な場合、ContainerとMeは動く" <|
                \_ ->
                    doMoveBoard (KeyDown Left)
                        { dummyModel
                            | matrixSize = 3
                            , board = Array.fromList [ Floor, Container, Me ]
                        }
                        |> Expect.equal (Array.fromList [ Container, Me, Floor ])
            , test "移動先:ContainerでContainerの移動先がWallの場合、ContainerとMeは動かない" <|
                \_ ->
                    doMoveBoard (KeyDown Left)
                        { dummyModel
                            | matrixSize = 3
                            , board = Array.fromList [ Wall, Container, Me ]
                        }
                        |> Expect.equal (Array.fromList [ Wall, Container, Me ])
            , test "移動先:ContainerでContainerの移動先がContainerの場合、ContainerとMeは動かない" <|
                \_ ->
                    doMoveBoard (KeyDown Left)
                        { dummyModel
                            | matrixSize = 3
                            , board = Array.fromList [ Container, Container, Me ]
                        }
                        |> Expect.equal (Array.fromList [ Container, Container, Me ])
            , test "移動先:ContainerでContainerの移動先にマスがない場合、ContainerとMeは動かない" <|
                \_ ->
                    doMoveBoard (KeyDown Left)
                        { dummyModel
                            | matrixSize = 2
                            , board = Array.fromList [ Container, Me ]
                        }
                        |> Expect.equal (Array.fromList [ Container, Me ])
            , test "移動先:Wallの場合、Meは動かない" <|
                \_ ->
                    doMoveBoard (KeyDown Left)
                        { dummyModel
                            | matrixSize = 2
                            , board = Array.fromList [ Wall, Me ]
                        }
                        |> Expect.equal (Array.fromList [ Wall, Me ])
            , test "移動先:マスがない場合、Meは動かない" <|
                \_ ->
                    doMoveBoard (KeyDown Left)
                        { dummyModel
                            | matrixSize = 2
                            , board = Array.fromList [ Me, Floor ]
                        }
                        |> Expect.equal (Array.fromList [ Me, Floor ])
            , test "移動元にベースセル指定がなかった場合、移動元はFloorに戻る" <|
                \_ ->
                    doMoveBoard (KeyDown Left)
                        { dummyModel
                            | matrixSize = 2
                            , board = Array.fromList [ Floor, Me ]
                        }
                        |> Array.get 1
                        |> Expect.equal (Just Floor)
            , test "移動元にベースセル指定があった場合、移動元はそのセルに戻る" <|
                \_ ->
                    doMoveBoard (KeyDown Left)
                        { dummyModel
                            | matrixSize = 2
                            , board = Array.fromList [ Floor, Me ]
                            , objectPlacement = Dict.fromList [ ( 1, Cord ) ]
                        }
                        |> Array.get 1
                        |> Expect.equal (Just Cord)
            , test "ゲーム続行中の場合、Meを動かせる" <|
                \_ ->
                    doMoveBoard (KeyDown Right)
                        { dummyModel
                            | matrixSize = 2
                            , gameStatus = Play
                            , board = Array.fromList [ Me, Floor, Floor, Floor ]
                        }
                        |> Expect.equal (Array.fromList [ Floor, Me, Floor, Floor ])
            , test "ゲームwaitの場合、Meは動かせない" <|
                \_ ->
                    doMoveBoard (KeyDown Right)
                        { dummyModel
                            | matrixSize = 2
                            , gameStatus = Clear
                            , board = Array.fromList [ Me, Floor, Floor, Floor ]
                        }
                        |> Expect.equal (Array.fromList [ Me, Floor, Floor, Floor ])
            ]
        ]


judgeGameStatusTest : Test
judgeGameStatusTest =
    describe "judgeGameStatus test"
        [ describe "aa"
            [ test "ゲーム続行" <|
                \_ ->
                    judgeGameStatus
                        (Dict.fromList
                            [ ( 0, Cord ) ]
                        )
                        (Array.fromList [ Cord ])
                        |> Expect.equal Play
            , test "ゲームクリア" <|
                \_ ->
                    judgeGameStatus
                        (Dict.fromList [ ( 0, Cord ) ])
                        (Array.fromList [ Container ])
                        |> Expect.equal Clear
            ]
        ]

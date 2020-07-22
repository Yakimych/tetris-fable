module App

open Elmish
open Elmish.React
open Feliz
open Tetris.Logic
open Tetris.Types

type Model = GameState

type Msg =
    | UpPressed
    | DownPressed
    | RightPressed
    | LeftPressed
    | RotatePressed
    | SpawnNextPiece // TODO: This should be an effect
    | LandPieceInPlace

let init () = GameLogic.initState ()

let update (msg: Msg) (model: Model): Model =
    match msg with
    | DownPressed -> model |> GameLogic.movePieceDown
    | UpPressed -> model |> GameLogic.movePieceUp
    | LeftPressed -> model |> GameLogic.movePieceLeft
    | RightPressed -> model |> GameLogic.movePieceRight
    | RotatePressed -> model |> GameLogic.rotatePiece
    | SpawnNextPiece -> model |> GameLogic.spawnNextPiece
    | LandPieceInPlace ->
        { model with
              Board =
                  model.Board
                  |> GameLogic.landPieceOnBoard model.CurrentPiece }

[<Literal>]
let PieceSizeOnBoard = 20

let canvasWidth = PieceSizeOnBoard * GameLogic.BoardWidth
let canvasHeight = PieceSizeOnBoard * GameLogic.BoardHeight


let drawCell (x: int) (y: int) (color: string) =
    Html.rect
        [ prop.width PieceSizeOnBoard
          prop.height PieceSizeOnBoard
          prop.x (x * PieceSizeOnBoard)
          prop.y (y * PieceSizeOnBoard)
          prop.style [ style.fill color ]
          prop.stroke "Black"
          prop.strokeWidth 1 ]

let drawBoard (board: BoardMap): Fable.React.ReactElement list =
    board
    |> Map.map (fun (x, y) _pieceType -> drawCell x y "Green")
    |> Map.toList
    |> List.map (fun (_, rect) -> rect)

let drawPiece (pieceState: PieceState) =
    getPieceSet pieceState.Shape pieceState.Orientation
    |> Set.toList
    |> List.map (fun (x, y) -> drawCell (pieceState.X + x) (pieceState.Y + y) "Blue") // TODO: Extract function to offset by pieceState.X and Y

let view (model: Model) (dispatch: Msg -> unit) =
    Html.div
        [ Html.button
            [ prop.onClick (fun _ -> dispatch UpPressed)
              prop.text "Up" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch LeftPressed)
                prop.text "Left" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch RightPressed)
                prop.text "Right" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch DownPressed)
                prop.text "Down" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch RotatePressed)
                prop.text "Rotate" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch SpawnNextPiece)
                prop.text "Spawn piece" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch LandPieceInPlace)
                prop.text "Land in place" ]

          Html.h6 "Board"
          Html.svg
              [ prop.viewBox (0, 0, canvasWidth, canvasHeight)
                prop.children
                    [ Html.rect
                        [ prop.width canvasWidth
                          prop.height canvasHeight
                          prop.x 0
                          prop.y 0
                          prop.style [ style.fill "LightGray" ]
                          prop.stroke "Black"
                          prop.strokeWidth 1 ]
                      yield! drawBoard model.Board
                      yield! drawPiece model.CurrentPiece ]
                unbox ("width", "200px") ]

          Html.h6 (sprintf "GameState: %A" model)
          Html.h6 (sprintf "PieceSet: %A" (getPieceSet model.CurrentPiece.Shape model.CurrentPiece.Orientation)) ]

Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run

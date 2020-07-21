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

let init () = GameLogic.initState ()

let update (msg: Msg) (model: Model): Model =
    match msg with
    | DownPressed -> model |> GameLogic.movePieceDown
    | UpPressed -> model |> GameLogic.movePieceUp
    | LeftPressed -> model |> GameLogic.movePieceLeft
    | RightPressed -> model |> GameLogic.movePieceRight
    | RotatePressed -> model |> GameLogic.rotatePiece

[<Literal>]
let canvasWidth = 800

[<Literal>]
let canvasHeight = 800

[<Literal>]
let pieceSizeOnBoard = 20

let drawCell (x: int) (y: int) (color: string) =
    Html.rect
        [ prop.width pieceSizeOnBoard
          prop.height pieceSizeOnBoard
          prop.x (x * pieceSizeOnBoard)
          prop.y (y * pieceSizeOnBoard)
          prop.style [ style.fill color ]
          prop.stroke "Red"
          prop.strokeWidth 2 ]

let drawPiece (pieceState: PieceState) =
    let pieceMatrix =
        getPieceMatrix pieceState.Shape pieceState.Orientation

    pieceMatrix
    |> Array.mapi (fun x column ->
        column
        |> Array.mapi (fun y hasCell ->
            if hasCell = 1 then
                drawCell (pieceState.X + x) (pieceState.Y + y) "Blue"
                |> Some
            else
                None))
    |> Array.concat
    |> Array.choose id

let view (model: Model) (dispatch: Msg -> unit) =
    Html.div
        [ Html.button
            [ prop.onClick (fun _ -> dispatch UpPressed)
              prop.text "Up" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch RightPressed)
                prop.text "Right" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch LeftPressed)
                prop.text "Left" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch DownPressed)
                prop.text "Down" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch RotatePressed)
                prop.text "Rotate" ]

          Html.h1 "Board"
          Html.svg
              [ prop.viewBox (0, 0, canvasWidth, canvasHeight)
                prop.children
                    [ Html.rect
                        [ prop.width canvasWidth
                          prop.height canvasHeight
                          prop.x 0
                          prop.y 0
                          prop.style [ style.fill "Orange" ]
                          prop.stroke "Red"
                          prop.strokeWidth 2 ]
                      yield! drawPiece model.CurrentPiece ]
                unbox ("width", "40%") ] ]

Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run

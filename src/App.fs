module App

open System
open Elmish
open Elmish.React
open Feliz
open Tetris.Logic
open Tetris.Types
open Tetris.Styling

type Model = GameState

type Msg =
    | Tick of DateTime
    | UpPressed
    | DownPressed
    | RightPressed
    | LeftPressed
    | SpawnNextPiece // TODO: This should be an effect
    | LandPieceInPlace
    | RemoveLines
    | PausePressed
    | ResumePressed
    | StartNewGamePressed
    | ToggleDebugPressed

[<Literal>]
let PieceSizeOnBoard = 20

[<Literal>]
let TickResolutionInMs = 10

let init () = GameLogic.initState ()

let update (msg: Msg) (model: Model): Model =
    match msg with
    | Tick _timeOfTick -> model |> GameLogic.tick TickResolutionInMs // TODO: Replace hardcoded TickResolution with calculation based on time
    | DownPressed -> model |> GameLogic.dropPiece
    | UpPressed -> model |> GameLogic.rotatePiece
    | LeftPressed -> model |> GameLogic.movePieceLeft
    | RightPressed -> model |> GameLogic.movePieceRight
    | SpawnNextPiece -> model |> GameLogic.spawnNextPiece
    | LandPieceInPlace ->
        { model with
              Board =
                  model.Board
                  |> GameLogic.landPieceOnBoard model.CurrentPiece }
    | RemoveLines -> model |> GameLogic.clearLines
    | PausePressed -> { model with TimerState = Paused }
    | ResumePressed -> { model with TimerState = Running }
    | StartNewGamePressed ->
        { GameLogic.initState () with
              TimerState = Running }
    | ToggleDebugPressed ->
        { model with
              ShowDebugInfo = not model.ShowDebugInfo }

let canvasWidth = PieceSizeOnBoard * GameLogic.BoardWidth
let canvasHeight = PieceSizeOnBoard * GameLogic.BoardHeight

let nextPieceCanvasSize = PieceSizeOnBoard * GameLogic.PieceSize

let drawCell (x: int) (y: int) (color: string) =
    Html.rect
        [ prop.width PieceSizeOnBoard
          prop.height PieceSizeOnBoard
          prop.x (x * PieceSizeOnBoard)
          prop.y (y * PieceSizeOnBoard)
          prop.style [ style.fill color ]
          prop.stroke "Black"
          prop.strokeWidth 1 ]

let drawPiece (atX: int) (atY: int) (pieceShape: PieceShape) (pieceOrientation: Orientation): ReactElement list =
    getPieceSet pieceShape pieceOrientation
    |> Set.toList
    |> List.map (fun (x, y) -> drawCell (atX + x) (atY + y) (getPieceColor pieceShape))

let drawPieceState (pieceState: PieceState) =
    drawPiece pieceState.X pieceState.Y pieceState.Shape pieceState.Orientation

let drawNextPieceCanvas (pieceShape: PieceShape): Fable.React.ReactElement list =
    let nextPieceCanvasRect =
        Html.rect
            [ prop.width nextPieceCanvasSize
              prop.height nextPieceCanvasSize
              prop.x 0
              prop.y 0
              prop.style [ style.fill BoardBackgroundColor ]
              prop.stroke BoardBorderColor
              prop.strokeWidth 1 ]

    let pieceTiles = drawPiece 0 0 pieceShape Up

    [ nextPieceCanvasRect
      yield! pieceTiles ]

let drawBackground (): Fable.React.ReactElement list =
    let boardRect =
        Html.rect
            [ prop.width canvasWidth
              prop.height canvasHeight
              prop.x 0
              prop.y 0
              prop.style [ style.fill BoardBackgroundColor ]
              prop.stroke BoardBorderColor
              prop.strokeWidth 1 ]

    let verticalLines =
        [ 1 .. GameLogic.BoardWidth - 1 ]
        |> Seq.map (fun col ->
            Html.line
                [ prop.x1 (col * PieceSizeOnBoard)
                  prop.y1 0
                  prop.x2 (col * PieceSizeOnBoard)
                  prop.y2 canvasHeight
                  prop.stroke LineColor
                  prop.strokeWidth 1 ])

    let horizontalLines =
        [ 1 .. GameLogic.BoardHeight - 1 ]
        |> Seq.map (fun row ->
            Html.line
                [ prop.x1 0
                  prop.y1 (row * PieceSizeOnBoard)
                  prop.x2 canvasWidth
                  prop.y2 (row * PieceSizeOnBoard)
                  prop.stroke LineColor
                  prop.strokeWidth 1 ])

    [ boardRect
      yield! verticalLines
      yield! horizontalLines ]

let drawBoard (board: BoardMap): Fable.React.ReactElement list =
    board
    |> Map.map (fun (x, y) boardTile -> drawCell x y (getTileColor boardTile))
    |> Map.toList
    |> List.map (fun (_, rect) -> rect)

let gameStatusText (timerState: TimerState): string =
    match timerState with
    | Running -> "Running"
    | Paused -> "Paused"
    | GameOver -> "Game Over"

let view (model: Model) (dispatch: Msg -> unit) =
    Html.div
        [ Html.button
            [ prop.onClick (fun _ -> dispatch StartNewGamePressed)
              prop.text "Start New Game" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch PausePressed)
                prop.text "Pause" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch ResumePressed)
                prop.text "Resume" ]

          Html.h6 (sprintf "%s. Lines cleared: %d" (gameStatusText model.TimerState) model.LinesCleared)
          Html.svg
              [ prop.viewBox (0, 0, canvasWidth, canvasHeight)
                prop.children
                    [ yield! drawBackground ()
                      yield! drawBoard model.Board
                      yield! drawPieceState model.CurrentPiece ]
                unbox ("width", "200px") ]

          Html.svg
              [ prop.viewBox (0, 0, nextPieceCanvasSize, nextPieceCanvasSize)
                prop.children (drawNextPieceCanvas model.NextShape)
                unbox ("width", "120px") ]

          Html.h6 "Debug"

          Html.button
              [ prop.onClick (fun _ -> dispatch ToggleDebugPressed)
                prop.text "Toggle Debug" ]
          if model.ShowDebugInfo then
              yield! [ Html.button
                           [ prop.onClick (fun _ -> dispatch SpawnNextPiece)
                             prop.text "Spawn piece" ]

                       Html.button
                           [ prop.onClick (fun _ -> dispatch LandPieceInPlace)
                             prop.text "Land in place" ]

                       Html.button
                           [ prop.onClick (fun _ -> dispatch RemoveLines)
                             prop.text "Remove Lines" ]

                       Html.span (sprintf "GameState: %A" model)
                       Html.span
                           (sprintf "PieceSet: %A" (getPieceSet model.CurrentPiece.Shape model.CurrentPiece.Orientation)) ] ]

let mergedSubscription initial =
    let timerSub dispatch =
        Browser.Dom.window.setInterval ((fun _ -> dispatch (Tick DateTime.UtcNow)), TickResolutionInMs, [||])
        |> ignore

    let keyPressSub (dispatch: Msg -> unit) =
        Browser.Dom.document.addEventListener
            ("keydown",
             (fun event ->
                 let keyboardEvent = event :?> Browser.Types.KeyboardEvent
                 match keyboardEvent.key with
                 | "ArrowUp" -> dispatch UpPressed
                 | "ArrowDown" -> dispatch DownPressed
                 | "ArrowLeft" -> dispatch LeftPressed
                 | "ArrowRight" -> dispatch RightPressed
                 | _ -> ()))

    Cmd.batch
        [ Cmd.ofSub timerSub
          Cmd.ofSub keyPressSub ]

Program.mkSimple init update view
|> Program.withSubscription mergedSubscription
|> Program.withReactSynchronous "elmish-app"
|> Program.run

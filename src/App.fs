module App

open System
open Elmish
open Elmish.React
open Feliz
open Tetris.Logic
open Tetris.Types
open Tetris.Styling

type Model = GameState

// TODO: Remove unused messages
type Msg =
    | Tick of DateTime
    | UpPressed
    | DownPressed
    | RightPressed
    | LeftPressed
    | RotatePressed
    | SpawnNextPiece // TODO: This should be an effect
    | LandPieceInPlace
    | RemoveLines
    | PausePressed
    | ResumePressed

[<Literal>]
let PieceSizeOnBoard = 20

[<Literal>]
let TickResolutionInMs = 10

let init () = GameLogic.initState ()

let update (msg: Msg) (model: Model): Model =
    match msg with
    | Tick _timeOfTick -> model |> GameLogic.tick TickResolutionInMs // TODO: Replace hardcoded TickResolution with calculation based on time
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
    | RemoveLines ->
        let newBoard = model.Board |> GameLogic.removeLines
        { model with Board = newBoard }
    | PausePressed -> { model with TimerState = Paused }
    | ResumePressed -> { model with TimerState = Running }

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

let drawPiece (pieceState: PieceState) =
    getPieceSet pieceState.Shape pieceState.Orientation
    |> Set.toList
    |> List.map (fun (x, y) -> drawCell (pieceState.X + x) (pieceState.Y + y) (getPieceColor pieceState.Shape)) // TODO: Extract function to offset by pieceState.X and Y

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

          Html.button
              [ prop.onClick (fun _ -> dispatch RemoveLines)
                prop.text "Remove Lines" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch PausePressed)
                prop.text "Pause" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch ResumePressed)
                prop.text "Resume" ]

          Html.h6 "Board"
          Html.svg
              [ prop.viewBox (0, 0, canvasWidth, canvasHeight)
                prop.children
                    [ yield! drawBackground ()
                      yield! drawBoard model.Board
                      yield! drawPiece model.CurrentPiece ]
                unbox ("width", "200px") ]

          Html.h6 (sprintf "GameState: %A" model)
          Html.h6 (sprintf "PieceSet: %A" (getPieceSet model.CurrentPiece.Shape model.CurrentPiece.Orientation)) ]

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
                 | "ArrowUp" -> dispatch RotatePressed
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

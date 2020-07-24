module App

open System
open Elmish
open Elmish.React
open Feliz
open Tetris.Logic
open Tetris.Types
open Tetris.Styling

type Model =
    | NotStarted
    | Playing of GameState
    | Paused of GameState
    | GameOver of GameState


type Msg =
    | Tick of DateTime
    | UpPressed
    | MovePieceDown
    | DownPressed
    | RightPressed
    | LeftPressed
    | SpawnNextPieceRequested
    | SpawnNextPiece of PieceShape
    | RemoveLines
    | PausePressed
    | ResumePressed
    | StartNewGamePressed
    | StartNewGame of startingPieceShape: PieceShape * nextPieceShape: PieceShape
    | ToggleDebugPressed

// LOGIC

[<Literal>]
let PieceSizeOnBoard = 20

[<Literal>]
let TickResolutionInMs = 10

let init () = NotStarted, Cmd.none

let getRandomShape () =
    match Random().Next(7) with
    | 0 -> T
    | 1 -> S
    | 2 -> Z
    | 3 -> I
    | 4 -> O
    | 5 -> L
    | 6 -> J
    | unexpectedRandomNumber ->
        failwithf "Something went wrong: the random number expected to be between 0 and 6. Actual: %d"
            unexpectedRandomNumber

let dispatchMovePieceDown (dispatch: Msg -> unit) = dispatch MovePieceDown

let spawnRandomPiece (dispatch: Msg -> unit) =
    dispatch <| SpawnNextPiece(getRandomShape ())

let startNewGame (dispatch: Msg -> unit) =
    let startingPieceShape = getRandomShape ()
    let nextPieceShape = getRandomShape ()

    dispatch
    <| StartNewGame(startingPieceShape, nextPieceShape)

let movePieceLeft (pieceState: PieceState) = { pieceState with X = pieceState.X - 1 }

let movePieceRight (pieceState: PieceState) = { pieceState with X = pieceState.X + 1 }

let getRotatedPiece (pieceState: PieceState) =
    { pieceState with
          Orientation =
              pieceState.Orientation
              |> GameLogic.nextOrientation }

let setPieceIfNoCollision (gameState: GameState) (newPiece: PieceState) =
    if newPiece
       |> GameLogic.hasCollisionWith gameState.Board then
        Playing gameState, Cmd.none
    else
        Playing
            ({ gameState with
                   CurrentPiece = newPiece }),
        Cmd.none

let update (msg: Msg) (model: Model) =
    match (model, msg) with
    | (Playing gameState, Tick _timeOfTick) ->
        let newMillisecondsSinceLastTick =
            gameState.MillisecondsSinceLastTick
            + TickResolutionInMs

        if newMillisecondsSinceLastTick > GameLogic.getTimerInterval gameState.LinesCleared then
            let stateWithResetElapsedTime =
                { gameState with
                      MillisecondsSinceLastTick = 0 }

            Playing(stateWithResetElapsedTime), Cmd.ofSub dispatchMovePieceDown
        else
            let stateWithUpdatedElapsedTime =
                { gameState with
                      MillisecondsSinceLastTick = newMillisecondsSinceLastTick }

            Playing(stateWithUpdatedElapsedTime), Cmd.none // TODO: Replace hardcoded TickResolution with calculation based on time

    | (Playing gameState, MovePieceDown)
    | (Playing gameState, DownPressed) ->
        let newPiece =
            { gameState.CurrentPiece with
                  Y = gameState.CurrentPiece.Y + 1 }

        if newPiece
           |> GameLogic.hasCollisionWith gameState.Board then

            let newGameState =
                gameState
                |> GameLogic.landPiece
                |> GameLogic.clearLines

            Playing(newGameState), Cmd.ofSub spawnRandomPiece
        else
            Playing
                ({ gameState with
                       CurrentPiece = newPiece }),
            Cmd.none

    | (Playing gameState, UpPressed) -> setPieceIfNoCollision gameState (gameState.CurrentPiece |> getRotatedPiece)
    | (Playing gameState, LeftPressed) -> setPieceIfNoCollision gameState (gameState.CurrentPiece |> movePieceLeft)
    | (Playing gameState, RightPressed) -> setPieceIfNoCollision gameState (gameState.CurrentPiece |> movePieceRight)

    | (Playing _, SpawnNextPieceRequested) -> model, Cmd.ofSub spawnRandomPiece
    | (Playing gameState, SpawnNextPiece nextPieceShape) ->
        let newCurrentPiece = GameLogic.initPiece gameState.NextShape

        if newCurrentPiece
           |> GameLogic.hasCollisionWith gameState.Board then
            GameOver gameState, Cmd.none
        else
            let newState =
                { gameState with
                      CurrentPiece = newCurrentPiece
                      NextShape = nextPieceShape }

            Playing newState, Cmd.none

    | (Playing gameState, RemoveLines) -> Playing(gameState |> GameLogic.clearLines), Cmd.none
    | (Playing gameState, PausePressed) -> Paused gameState, Cmd.none

    | (Paused gameState, ResumePressed) -> Playing gameState, Cmd.none

    | (_, StartNewGamePressed) -> NotStarted, Cmd.ofSub startNewGame
    | (_, StartNewGame (startingPieceShape, nextPieceShape)) ->
        Playing(GameLogic.initState startingPieceShape nextPieceShape), Cmd.none

    | (Paused gameState, ToggleDebugPressed) ->
        Paused
            { gameState with
                  ShowDebugInfo = not gameState.ShowDebugInfo },
        Cmd.none

    | (_, _) -> model, Cmd.none


// VIEW

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

let linesClearedText gameState =
    sprintf "Lines cleared: %d" gameState.LinesCleared

let gameStatusText (model: Model): string =
    match model with
    | NotStarted -> "Not Started"
    | Playing gameState ->
        sprintf "Playing. %s"
        <| linesClearedText gameState
    | Paused gameState -> sprintf "Paused. %s" <| linesClearedText gameState
    | GameOver gameState ->
        sprintf "Game Over! %s"
        <| linesClearedText gameState // TODO: Show final score

let debugPanel (model: Model) (dispatch: Msg -> unit): ReactElement list =
    match model with
    | Paused gameState when gameState.ShowDebugInfo ->
        [ Html.button
            [ prop.onClick (fun _ -> dispatch RemoveLines)
              prop.text "Remove Lines" ]

          Html.span (sprintf "GameState: %A" model)
          Html.span
              (sprintf "PieceSet: %A" (getPieceSet gameState.CurrentPiece.Shape gameState.CurrentPiece.Orientation)) ]
    | _ -> []

let view (model: Model) (dispatch: Msg -> unit) =
    let tilesOnMainCanvas =
        match model with
        | NotStarted -> []
        | Playing gameState
        | Paused gameState
        | GameOver gameState ->
            [ yield! drawBoard gameState.Board
              yield! drawPieceState gameState.CurrentPiece ]

    let tilesOnNextPieceCanvas =
        match model with
        | NotStarted -> []
        | Playing gameState
        | Paused gameState
        | GameOver gameState -> drawNextPieceCanvas gameState.NextShape

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

          Html.h6 (gameStatusText model)
          Html.svg
              [ prop.viewBox (0, 0, canvasWidth, canvasHeight)
                prop.children
                    [ yield! drawBackground ()
                      yield! tilesOnMainCanvas ]
                unbox ("width", "200px") ]

          Html.svg
              [ prop.viewBox (0, 0, nextPieceCanvasSize, nextPieceCanvasSize)
                prop.children tilesOnNextPieceCanvas
                unbox ("width", "120px") ]

          Html.h6 "Debug"

          Html.button
              [ prop.onClick (fun _ -> dispatch ToggleDebugPressed)
                prop.text "Toggle Debug" ]
          yield! (debugPanel model dispatch) ]

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

Program.mkProgram init update view
|> Program.withSubscription mergedSubscription
|> Program.withReactSynchronous "elmish-app"
|> Program.run

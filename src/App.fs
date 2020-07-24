module App

open System
open Elmish
open Elmish.React
open Feliz
open Tetris.Types
open Tetris.Styling

type PieceState =
    { Shape: PieceShape
      Orientation: Orientation
      X: int
      Y: int }

type GameState =
    { Board: BoardMap
      CurrentPiece: PieceState
      NextShape: PieceShape
      MillisecondsSinceLastTick: int
      LinesCleared: int }

type Model =
    | NotStarted
    | Running of GameState
    | Paused of GameState
    | GameOver of GameState

type Msg =
    | Tick of DateTime
    | UpPressed
    | MovePieceDown
    | DownPressed
    | RightPressed
    | LeftPressed
    | SpawnNextPiece of PieceShape
    | PausePressed
    | ResumePressed
    | StartNewGamePressed
    | StartNewGame of startingPieceShape: PieceShape * nextPieceShape: PieceShape

[<AutoOpen>]
module Logic =
    [<Literal>]
    let PieceSizeOnBoard = 20

    [<Literal>]
    let TickResolutionInMs = 10

    [<Literal>]
    let BoardWidth = 10

    [<Literal>]
    let BoardHeight = 20

    [<Literal>]
    let PieceSize = 4

    [<Literal>]
    let ClearedLinesBetweenLevelIncreases = 10

    let timerIntervalsPerLevel = [| 500; 400; 300; 200; 100; 75; 50 |]

    let addSideBoundaries (board: BoardMap): BoardMap =
        [ -1 .. BoardHeight ]
        |> Seq.fold (fun tempBoard y ->
            tempBoard
            |> Map.add (-1, y) Boundary
            |> Map.add (BoardWidth, y) Boundary) board

    let addBottomBoundary (board: BoardMap): BoardMap =
        [ 0 .. BoardWidth ]
        |> Seq.fold (fun tempBoard x -> tempBoard |> Map.add (x, BoardHeight) Boundary) board

    let addBoundaries: BoardMap -> BoardMap = addSideBoundaries >> addBottomBoundary

    let initPiece (pieceShape: PieceShape): PieceState =
        { Shape = pieceShape
          Orientation = Up
          X = 3
          Y = -2 }

    let initState (startingPieceShape: PieceShape) (nextPieceShape: PieceShape): GameState =
        { Board = Map.empty |> addBoundaries
          CurrentPiece = initPiece <| startingPieceShape
          NextShape = nextPieceShape
          MillisecondsSinceLastTick = 0 // TODO: TimeSpan?
          LinesCleared = 0 }

    let landPiece (gameState: GameState): GameState =
        let newBoard =
            getPieceSet gameState.CurrentPiece.Shape gameState.CurrentPiece.Orientation
            |> Set.fold (fun tempBoard (x, y) ->
                tempBoard
                |> Map.add (x + gameState.CurrentPiece.X, y + gameState.CurrentPiece.Y)
                       (OccupiedBy gameState.CurrentPiece.Shape)) gameState.Board

        { gameState with Board = newBoard }

    let hasCollisionWith (board: BoardMap) (piece: PieceState): bool =
        let pieceSet =
            getPieceSet piece.Shape piece.Orientation
            |> Set.map (fun (x, y) -> (x + piece.X, y + piece.Y))

        let boardSet =
            board
            |> Map.toSeq
            |> Seq.map (fun ((x, y), _) -> (x, y))
            |> Set.ofSeq

        boardSet
        |> Set.intersect pieceSet
        |> Set.isEmpty
        |> not

    let isOccupiedByPiece (boardTile: BoardTile): bool =
        match boardTile with
        | OccupiedBy _ -> true
        | Boundary -> false

    let isBoundary (boardTile: BoardTile): bool =
        match boardTile with
        | OccupiedBy _ -> false
        | Boundary -> true

    let isFull (board: BoardMap) (line: int): bool =
        let tilesOnLine =
            board
            |> Map.toList
            |> List.where (fun ((_, y), boardTile) -> isOccupiedByPiece boardTile && y = line)
            |> List.length

        tilesOnLine = BoardWidth

    let tryFindBottomMostFullLine (board: BoardMap): int option =
        [ 0 .. BoardHeight - 1 ]
        |> Seq.tryFindIndexBack (fun line -> line |> isFull board)

    let removeLine (board: BoardMap) (line: int): BoardMap =
        board
        |> Map.filter (fun (_, y) boardTile -> y <> line || boardTile |> isBoundary)
        |> Map.toList
        |> List.map (fun ((x, y), boardTile) ->
            let shiftedY =
                if y < line && boardTile |> isOccupiedByPiece
                then y + 1
                else y

            ((x, shiftedY), boardTile))
        |> Map.ofList

    let rec clearLines (gameState: GameState): GameState =
        match gameState.Board |> tryFindBottomMostFullLine with
        | Some lineToRemove ->
            let newBoard = removeLine gameState.Board lineToRemove
            clearLines
                { gameState with
                      Board = newBoard
                      LinesCleared = gameState.LinesCleared + 1 }
        | None -> gameState

    let getTimerIntervalForCurrentLevel (linesCleared: int): int =
        let level =
            linesCleared / ClearedLinesBetweenLevelIncreases

        timerIntervalsPerLevel.[level]

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

    let spawnRandomPieceCmd =
        getRandomShape >> SpawnNextPiece >> Cmd.ofMsg

    let startNewGameCmd () =
        let startingPieceShape = getRandomShape ()
        let nextPieceShape = getRandomShape ()

        StartNewGame(startingPieceShape, nextPieceShape)
        |> Cmd.ofMsg

    let movedLeft (pieceState: PieceState) = { pieceState with X = pieceState.X - 1 }

    let movedRight (pieceState: PieceState) = { pieceState with X = pieceState.X + 1 }

    let getRotatedPiece (pieceState: PieceState) =
        { pieceState with
              Orientation = pieceState.Orientation |> getNextOrientation }

    let setPieceIfNoCollision (gameState: GameState) (newPiece: PieceState) =
        if newPiece |> hasCollisionWith gameState.Board then
            Running gameState, Cmd.none
        else
            Running
                ({ gameState with
                       CurrentPiece = newPiece }),
            Cmd.none

    let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
        match (model, msg) with
        | (Running gameState, Tick _timeOfTick) ->
            let newMillisecondsSinceLastTick =
                gameState.MillisecondsSinceLastTick
                + TickResolutionInMs

            if newMillisecondsSinceLastTick > getTimerIntervalForCurrentLevel gameState.LinesCleared then
                let stateWithResetElapsedTime =
                    { gameState with
                          MillisecondsSinceLastTick = 0 }

                Running(stateWithResetElapsedTime), Cmd.ofMsg MovePieceDown
            else
                let stateWithUpdatedElapsedTime =
                    { gameState with
                          MillisecondsSinceLastTick = newMillisecondsSinceLastTick }

                Running(stateWithUpdatedElapsedTime), Cmd.none // TODO: Replace hardcoded TickResolution with calculation based on time

        | (Running gameState, MovePieceDown)
        | (Running gameState, DownPressed) ->
            let newPiece =
                { gameState.CurrentPiece with
                      Y = gameState.CurrentPiece.Y + 1 }

            if newPiece |> hasCollisionWith gameState.Board then
                Running(gameState |> landPiece |> clearLines), spawnRandomPieceCmd ()
            else
                Running
                    ({ gameState with
                           CurrentPiece = newPiece }),
                Cmd.none

        | (Running gameState, UpPressed) -> setPieceIfNoCollision gameState (gameState.CurrentPiece |> getRotatedPiece)
        | (Running gameState, LeftPressed) -> setPieceIfNoCollision gameState (gameState.CurrentPiece |> movedLeft)
        | (Running gameState, RightPressed) -> setPieceIfNoCollision gameState (gameState.CurrentPiece |> movedRight)

        | (Running gameState, SpawnNextPiece nextPieceShape) ->
            let newPiece = initPiece gameState.NextShape

            if newPiece |> hasCollisionWith gameState.Board then
                GameOver gameState, Cmd.none
            else
                let newState =
                    { gameState with
                          CurrentPiece = newPiece
                          NextShape = nextPieceShape }

                Running newState, Cmd.none

        | (Running gameState, PausePressed) -> Paused gameState, Cmd.none
        | (Paused gameState, ResumePressed) -> Running gameState, Cmd.none

        | (_, StartNewGamePressed) -> NotStarted, startNewGameCmd ()
        | (_, StartNewGame (startingPieceShape, nextPieceShape)) ->
            Running(initState startingPieceShape nextPieceShape), Cmd.none

        | (_, _) -> model, Cmd.none


[<AutoOpen>]
module View =
    let canvasWidth = PieceSizeOnBoard * BoardWidth
    let canvasHeight = PieceSizeOnBoard * BoardHeight

    let nextPieceCanvasSize = PieceSizeOnBoard * PieceSize

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
            [ 1 .. BoardWidth - 1 ]
            |> Seq.map (fun col ->
                Html.line
                    [ prop.x1 (col * PieceSizeOnBoard)
                      prop.y1 0
                      prop.x2 (col * PieceSizeOnBoard)
                      prop.y2 canvasHeight
                      prop.stroke LineColor
                      prop.strokeWidth 1 ])

        let horizontalLines =
            [ 1 .. BoardHeight - 1 ]
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
        | Running gameState ->
            sprintf "Running. %s"
            <| linesClearedText gameState
        | Paused gameState -> sprintf "Paused. %s" <| linesClearedText gameState
        | GameOver gameState ->
            sprintf "Game Over! %s"
            <| linesClearedText gameState // TODO: Show final score

    let view (model: Model) (dispatch: Msg -> unit) =
        let tilesOnMainCanvas =
            match model with
            | NotStarted -> []
            | Running gameState
            | Paused gameState
            | GameOver gameState ->
                [ yield! drawBoard gameState.Board
                  yield! drawPieceState gameState.CurrentPiece ]

        let tilesOnNextPieceCanvas =
            match model with
            | NotStarted -> []
            | Running gameState
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
                    unbox ("width", "120px") ] ]

    let mergedSubscription _ =
        let timerSub (dispatch: Msg -> unit): unit =
            Browser.Dom.window.setInterval ((fun _ -> dispatch (Tick DateTime.UtcNow)), TickResolutionInMs, [||])
            |> ignore

        let keyPressSub (dispatch: Msg -> unit): unit =
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

module TetrisProgram =
    Program.mkProgram init update view
    |> Program.withSubscription mergedSubscription
    |> Program.withReactSynchronous "elmish-app"
    |> Program.run

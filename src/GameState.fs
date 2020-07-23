namespace Tetris.Logic

open System
open Tetris.Types

type PieceState =
    { Shape: PieceShape
      Orientation: Orientation
      X: int
      Y: int }

type TimerState =
    | Running
    | Paused

type GameState =
    { Board: BoardMap
      CurrentPiece: PieceState
      NextShape: PieceShape
      MillisecondsSinceLastTick: int
      LinesCleared: int
      TimerState: TimerState
      ShowDebugInfo: bool }

module GameLogic =
    [<Literal>]
    let BoardWidth = 10

    [<Literal>]
    let BoardHeight = 20

    [<Literal>]
    let PieceSize = 4

    let getRandomShape (): PieceShape =
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

    let nextOrientation (currentOrientation: Orientation): Orientation =
        match currentOrientation with
        | Up -> Left
        | Left -> Down
        | Down -> Right
        | Right -> Up

    let addLeftBoundary (board: BoardMap): BoardMap =
        [ -1 .. BoardHeight ]
        |> Seq.fold (fun tempBoard y -> tempBoard |> Map.add (-1, y) Boundary) board

    let addBottomBoundary (board: BoardMap): BoardMap =
        [ 0 .. BoardWidth ]
        |> Seq.fold (fun tempBoard x -> tempBoard |> Map.add (x, BoardHeight) Boundary) board

    let addRightBoundary (board: BoardMap): BoardMap =
        [ -1 .. BoardHeight ]
        |> Seq.fold (fun tempBoard y -> tempBoard |> Map.add (BoardWidth, y) Boundary) board

    let addBoundaries (board: BoardMap): BoardMap =
        board
        |> addLeftBoundary
        |> addRightBoundary
        |> addBottomBoundary

    let resetPiece (pieceShape: PieceShape) =
        { Shape = pieceShape
          Orientation = Up
          X = 3
          Y = -2 }

    let initState () =
        { Board = Map.empty |> addBoundaries
          CurrentPiece = resetPiece <| getRandomShape ()
          NextShape = getRandomShape ()
          MillisecondsSinceLastTick = 0 // TODO: TimeSpan?
          LinesCleared = 0
          TimerState = Paused
          ShowDebugInfo = false }

    let landPieceOnBoard (piece: PieceState) (board: BoardMap) =
        getPieceSet piece.Shape piece.Orientation
        |> Set.fold (fun tempBoard (x, y) ->
            tempBoard
            |> Map.add (x + piece.X, y + piece.Y) (OccupiedBy piece.Shape)) board

    let hasCollisionWith (board: BoardMap) (piece: PieceState) =
        let pieceSet =
            getPieceSet piece.Shape piece.Orientation
            |> Set.map (fun (x, y) -> (x + piece.X, y + piece.Y))

        let boardSet =
            board
            |> Map.toSeq
            |> Seq.map (fun ((x, y), _) -> (x, y))
            |> Set.ofSeq

        let intersectionSet = boardSet |> Set.intersect pieceSet
        intersectionSet |> Set.isEmpty |> not

    // TODO: This should be a Cmd/effect
    let spawnNextPiece (gameState: GameState): GameState =
        { gameState with
              CurrentPiece = resetPiece gameState.NextShape
              NextShape = getRandomShape () }

    let isOccupiedByPiece (boardTile: BoardTile): bool =
        match boardTile with
        | OccupiedBy _ -> true
        | Boundary -> false

    let isBoundary (boardTile: BoardTile): bool =
        match boardTile with
        | OccupiedBy _ -> false
        | Boundary -> true

    let isFull (board: BoardMap) (line: int) =
        let tilesOnLine =
            board
            |> Map.toList
            |> List.where (fun ((_, y), boardTile) -> isOccupiedByPiece boardTile && y = line)
            |> List.length

        tilesOnLine = BoardWidth

    let tryFindBottomMostFullLine (board: BoardMap): int option =
        [ 0 .. BoardHeight - 1 ]
        |> Seq.tryFindIndexBack (fun line -> line |> isFull board)

    let removeLine (board: BoardMap) (line: int) =
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

    let updateIfNoCollisionWith (newPiece: PieceState) (gameState: GameState) =
        if newPiece |> hasCollisionWith gameState.Board then
            gameState
        else
            { gameState with
                  CurrentPiece = newPiece }

    let processPieceLanded (gameState: GameState): GameState =
        let newBoard =
            gameState.Board
            |> landPieceOnBoard gameState.CurrentPiece

        { gameState with Board = newBoard }
        |> clearLines
        |> spawnNextPiece

    let rec dropPiece (gameState: GameState): GameState =
        let newPiece =
            { gameState.CurrentPiece with
                  Y = gameState.CurrentPiece.Y + 1 }

        if newPiece |> hasCollisionWith gameState.Board then
            gameState |> processPieceLanded
        else
            { gameState with
                  CurrentPiece = newPiece }
            |> dropPiece

    let movePieceDown (gameState: GameState): GameState =
        let newPiece =
            { gameState.CurrentPiece with
                  Y = gameState.CurrentPiece.Y + 1 }

        if newPiece |> hasCollisionWith gameState.Board then
            gameState |> processPieceLanded
        else
            { gameState with
                  CurrentPiece = newPiece }

    let movePieceLeft (gameState: GameState): GameState =
        let newPiece =
            { gameState.CurrentPiece with
                  X = gameState.CurrentPiece.X - 1 }

        gameState |> updateIfNoCollisionWith newPiece

    let movePieceRight (gameState: GameState): GameState =
        let newPiece =
            { gameState.CurrentPiece with
                  X = gameState.CurrentPiece.X + 1 }

        gameState |> updateIfNoCollisionWith newPiece

    let rotatePiece (gameState: GameState): GameState =
        let newPiece =
            { gameState.CurrentPiece with
                  Orientation =
                      gameState.CurrentPiece.Orientation
                      |> nextOrientation }

        gameState |> updateIfNoCollisionWith newPiece

    let timerIntervalsPerLevel = [| 1000; 500; 400; 300; 200; 100; 50 |]

    [<Literal>]
    let ClearedLinesBetweenLevelIncreases = 10

    let timerInterval (linesCleared: int): int =
        let level =
            linesCleared / ClearedLinesBetweenLevelIncreases

        timerIntervalsPerLevel.[level]

    let tick (milliseconds: int) (gameState: GameState): GameState =
        // TODO: Move out of the tick function?
        match gameState.TimerState with
        | Running ->
            let newMillisecondsSinceLastTick =
                gameState.MillisecondsSinceLastTick + milliseconds

            if newMillisecondsSinceLastTick > timerInterval gameState.LinesCleared then
                { gameState with
                      MillisecondsSinceLastTick = 0 }
                |> movePieceDown
            else
                { gameState with
                      MillisecondsSinceLastTick = newMillisecondsSinceLastTick }
        | Paused -> gameState

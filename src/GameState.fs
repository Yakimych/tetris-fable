namespace Tetris.Logic

open System
open Tetris.Types

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
      LinesCleared: int
      ShowDebugInfo: bool }

module GameLogic =
    [<Literal>]
    let BoardWidth = 10

    [<Literal>]
    let BoardHeight = 20

    [<Literal>]
    let PieceSize = 4

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
          LinesCleared = 0
          ShowDebugInfo = false }

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

    let timerIntervalsPerLevel = [| 500; 400; 300; 200; 100; 75; 50 |]

    [<Literal>]
    let ClearedLinesBetweenLevelIncreases = 10

    let getTimerInterval (linesCleared: int): int =
        let level =
            linesCleared / ClearedLinesBetweenLevelIncreases

        timerIntervalsPerLevel.[level]

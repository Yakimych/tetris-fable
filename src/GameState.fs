namespace Tetris.Logic

open Tetris.Types

type PieceState =
    { Shape: PieceShape
      Orientation: Orientation
      X: int
      Y: int }

type BoardTile =
    | OccupiedBy of PieceShape
    | Boundary

type BoardMap = Map<int * int, BoardTile>

type GameState =
    { Board: BoardMap
      CurrentPiece: PieceState }

module GameLogic =
    [<Literal>]
    let BoardWidth = 10

    [<Literal>]
    let BoardHeight = 20

    [<Literal>]
    let PieceSize = 4

    let getRandomPiece () =
        let randomNumber = System.Random().Next(7)

        let shape =
            match randomNumber with
            | 0 -> T
            | 1 -> S
            | 2 -> Z
            | 3 -> I
            | 4 -> O
            | 5 -> L
            | 6 -> J
            | _ ->
                failwithf "Something went wrong: the random number expected to be between 0 and 6. Actual: %d"
                    randomNumber

        { Shape = shape
          Orientation = Up
          X = 5
          Y = 2 }

    let nextOrientation (currentOrientation: Orientation): Orientation =
        match currentOrientation with
        | Up -> Left
        | Left -> Down
        | Down -> Right
        | Right -> Up

    let addLeftBoundary (board: BoardMap): BoardMap =
        [ 0 .. BoardHeight ]
        |> Seq.fold (fun tempBoard y -> tempBoard |> Map.add (-1, y) Boundary) board

    let addBottomBoundary (board: BoardMap): BoardMap =
        [ 0 .. BoardWidth ]
        |> Seq.fold (fun tempBoard x -> tempBoard |> Map.add (x, BoardHeight) Boundary) board

    let addRightBoundary (board: BoardMap): BoardMap =
        [ 0 .. BoardHeight ]
        |> Seq.fold (fun tempBoard y -> tempBoard |> Map.add (BoardWidth, y) Boundary) board

    let addBoundaries (board: BoardMap): BoardMap =
        board
        |> addLeftBoundary
        |> addRightBoundary
        |> addBottomBoundary

    let initState () =
        { Board = Map.empty |> addBoundaries
          CurrentPiece = getRandomPiece () }

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

    let spawnNextPiece (gameState: GameState): GameState =
        { gameState with
              CurrentPiece = getRandomPiece () }

    let updateIfNoCollisionWith (newPiece: PieceState) (gameState: GameState) =
        if newPiece |> hasCollisionWith gameState.Board then
            gameState
        else
            { gameState with
                  CurrentPiece = newPiece }

    let movePieceDown (gameState: GameState): GameState =
        let newPiece =
            { gameState.CurrentPiece with
                  Y = gameState.CurrentPiece.Y + 1 }

        gameState |> updateIfNoCollisionWith newPiece

    let movePieceUp (gameState: GameState): GameState =
        let newPiece =
            { gameState.CurrentPiece with
                  Y = gameState.CurrentPiece.Y - 1 }

        gameState |> updateIfNoCollisionWith newPiece

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

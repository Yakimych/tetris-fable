namespace Tetris.Logic

open Tetris.Types

type PieceState =
    { Shape: PieceShape
      Orientation: Orientation
      X: int
      Y: int }

type BoardArray = int array array

type GameState =
    { Board: BoardArray
      CurrentPiece: PieceState }

module GameLogic =
    [<Literal>]
    let BoardWidth = 10

    [<Literal>]
    let BoardHeight = 20

    [<Literal>]
    let PieceSize = 4

    let getRandomPiece () =
        { Shape = I
          Orientation = Up
          X = 5
          Y = 2 }

    let nextOrientation (currentOrientation: Orientation): Orientation =
        match currentOrientation with
        | Up -> Left
        | Left -> Down
        | Down -> Right
        | Right -> Up

    let initState () =
        let line = [| 0 .. BoardWidth |]

        let lines =
            [| 0 .. BoardHeight |]
            |> Array.map (fun _ -> line)

        { Board = lines
          CurrentPiece = getRandomPiece () }

    let movePieceDown (gameState: GameState): GameState =
        let newPiece =
            { gameState.CurrentPiece with
                  Y = gameState.CurrentPiece.Y + 1 }

        { gameState with
              CurrentPiece = newPiece }

    let movePieceUp (gameState: GameState): GameState =
        let newPiece =
            { gameState.CurrentPiece with
                  Y = gameState.CurrentPiece.Y - 1 }

        { gameState with
              CurrentPiece = newPiece }

    let movePieceLeft (gameState: GameState): GameState =
        let newPiece =
            { gameState.CurrentPiece with
                  X = gameState.CurrentPiece.X - 1 }

        { gameState with
              CurrentPiece = newPiece }

    let movePieceRight (gameState: GameState): GameState =
        let newPiece =
            { gameState.CurrentPiece with
                  X = gameState.CurrentPiece.X + 1 }

        { gameState with
              CurrentPiece = newPiece }

    let rotatePiece (gameState: GameState): GameState =
        let newPiece =
            { gameState.CurrentPiece with
                  Orientation =
                      gameState.CurrentPiece.Orientation
                      |> nextOrientation }

        { gameState with
              CurrentPiece = newPiece }

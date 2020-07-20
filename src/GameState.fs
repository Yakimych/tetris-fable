namespace Tetris.Logic

open Tetris.Types

type PieceState =
    { Shape: PieceShape
      Orientation: Orientation
      X: int
      Y: int }

type BoardArray = (int * int) [,]

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
          Y = 0 }

    let nextOrientation (currentOrientation: Orientation): Orientation =
        match currentOrientation with
        | Up -> Left
        | Left -> Down
        | Down -> Right
        | Right -> Up

    let initState () =
        { Board = Array2D.zeroCreate BoardWidth BoardHeight
          CurrentPiece = getRandomPiece () }

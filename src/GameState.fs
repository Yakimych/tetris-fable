namespace Tetris

type PieceState =
    { Shape: PieceShape
      Orientation: Orientation
      X: int
      Y: int }

type GameState =
    { Board: (int * int) [,]
      CurrentPiece: PieceState }

module GameLogic =
    [<Literal>]
    let BoardWidth = 10

    [<Literal>]
    let BoardHeight = 20

    let getRandomPiece () =
        { Shape = PieceShape.I
          Orientation = Up
          X = 5
          Y = 0 }

    let initState () =
        { Board = Array2D.init BoardWidth BoardHeight (fun x y -> (x, y))
          CurrentPiece = getRandomPiece () }

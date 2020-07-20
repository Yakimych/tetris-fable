module Tetris

type PieceShape =
    | T
    | S
    | Z
    | I
    | O
    | L
    | J

type Orientation =
    | Up
    | Left
    | Down
    | Right

module PieceI =
    let getMatrix (orientation: Orientation) =
        match orientation with
        | Up
        | Down ->
            [| [| 0; 0; 1; 0 |]
               [| 0; 0; 1; 0 |]
               [| 0; 0; 1; 0 |]
               [| 0; 0; 1; 0 |] |]
        | Left
        | Right ->
            [| [| 0; 0; 0; 0 |]
               [| 1; 1; 1; 1 |]
               [| 0; 0; 0; 0 |]
               [| 0; 0; 0; 0 |] |]

let getPieceMatrix (pieceShape: PieceShape) (orientation: Orientation) =
    match pieceShape with
    | I -> PieceI.getMatrix orientation
    // TODO
    | _ -> PieceI.getMatrix orientation

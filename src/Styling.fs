module Tetris.Styling

open Types

[<Literal>]
let LineColor = "DarkGray"

[<Literal>]
let BoardBackgroundColor = "LightGray"

[<Literal>]
let BoardBorderColor = "Black"

let getPieceColor (pieceShape: PieceShape): string =
    match pieceShape with
    | T -> "Green"
    | S -> "Orange"
    | Z -> "Purple"
    | I -> "Brown"
    | O -> "Yellow"
    | L -> "Blue"
    | J -> "Pink"

let getLandedPieceColor (pieceShape: PieceShape): string =
    match pieceShape with
    | T -> "DarkGreen"
    | S -> "DarkOrange"
    | Z -> "Indigo"
    | I -> "Maroon"
    | O -> "Khaki"
    | L -> "Blue"
    | J -> "Violet"

let getTileColor (boardTile: BoardTile): string =
    match boardTile with
    | OccupiedBy piece -> getLandedPieceColor piece
    | Boundary -> "Black"

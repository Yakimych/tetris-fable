module Tetris.Types

type PieceSet = Set<int * int>

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
               [| 0; 0; 0; 0 |]
               [| 1; 1; 1; 1 |]
               [| 0; 0; 0; 0 |] |]

module PieceO =
    let getMatrix () =
        [| [| 0; 0; 0; 0 |]
           [| 0; 1; 1; 0 |]
           [| 0; 1; 1; 0 |]
           [| 0; 0; 0; 0 |] |]

module PieceJ =
    let getMatrix (orientation: Orientation) =
        match orientation with
        | Up ->
            [| [| 0; 0; 0; 0 |]
               [| 1; 0; 0; 0 |]
               [| 1; 1; 1; 0 |]
               [| 0; 0; 0; 0 |] |]
        | Left ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 1; 0; 0 |]
               [| 0; 1; 0; 0 |]
               [| 1; 1; 0; 0 |] |]
        | Down ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 0; 0; 0 |]
               [| 1; 1; 1; 0 |]
               [| 0; 0; 1; 0 |] |]
        | Right ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 1; 1; 0 |]
               [| 0; 1; 0; 0 |]
               [| 0; 1; 0; 0 |] |]

module PieceL =
    let getMatrix (orientation: Orientation) =
        match orientation with
        | Up ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 0; 1; 0 |]
               [| 1; 1; 1; 0 |]
               [| 0; 0; 0; 0 |] |]
        | Left ->
            [| [| 0; 0; 0; 0 |]
               [| 1; 1; 0; 0 |]
               [| 0; 1; 0; 0 |]
               [| 0; 1; 0; 0 |] |]
        | Down ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 0; 0; 0 |]
               [| 1; 1; 1; 0 |]
               [| 1; 0; 0; 0 |] |]
        | Right ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 1; 0; 0 |]
               [| 0; 1; 0; 0 |]
               [| 0; 1; 1; 0 |] |]

module PieceS =
    let getMatrix (orientation: Orientation) =
        match orientation with
        | Up ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 1; 1; 0 |]
               [| 1; 1; 0; 0 |]
               [| 0; 0; 0; 0 |] |]
        | Left ->
            [| [| 0; 0; 0; 0 |]
               [| 1; 0; 0; 0 |]
               [| 1; 1; 0; 0 |]
               [| 0; 1; 0; 0 |] |]
        | Down ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 0; 0; 0 |]
               [| 0; 1; 1; 0 |]
               [| 1; 1; 0; 0 |] |]
        | Right ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 1; 0; 0 |]
               [| 0; 1; 1; 0 |]
               [| 0; 0; 1; 0 |] |]

module PieceZ =
    let getMatrix (orientation: Orientation) =
        match orientation with
        | Up ->
            [| [| 0; 0; 0; 0 |]
               [| 1; 1; 0; 0 |]
               [| 0; 1; 1; 0 |]
               [| 0; 0; 0; 0 |] |]
        | Left ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 1; 0; 0 |]
               [| 1; 1; 0; 0 |]
               [| 1; 0; 0; 0 |] |]
        | Down ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 0; 0; 0 |]
               [| 1; 1; 0; 0 |]
               [| 0; 1; 1; 0 |] |]
        | Right ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 0; 1; 0 |]
               [| 0; 1; 1; 0 |]
               [| 0; 1; 0; 0 |] |]

module PieceT =
    let getMatrix (orientation: Orientation) =
        match orientation with
        | Up ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 1; 0; 0 |]
               [| 1; 1; 1; 0 |]
               [| 0; 0; 0; 0 |] |]
        | Left ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 1; 0; 0 |]
               [| 1; 1; 0; 0 |]
               [| 0; 1; 0; 0 |] |]
        | Down ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 0; 0; 0 |]
               [| 1; 1; 1; 0 |]
               [| 0; 1; 0; 0 |] |]
        | Right ->
            [| [| 0; 0; 0; 0 |]
               [| 0; 1; 0; 0 |]
               [| 0; 1; 1; 0 |]
               [| 0; 1; 0; 0 |] |]

let getPieceSet (pieceShape: PieceShape) (orientation: Orientation): PieceSet =
    let matrix =
        match pieceShape with
        | I -> PieceI.getMatrix orientation
        | O -> PieceO.getMatrix ()
        | J -> PieceJ.getMatrix orientation
        | L -> PieceL.getMatrix orientation
        | S -> PieceS.getMatrix orientation
        | Z -> PieceZ.getMatrix orientation
        | T -> PieceT.getMatrix orientation

    matrix
    |> Array.mapi (fun y row ->
        row
        |> Array.mapi (fun x cellValue -> if cellValue = 1 then (x, y) |> Some else None))
    |> Array.concat
    |> Array.choose id
    |> Set.ofArray

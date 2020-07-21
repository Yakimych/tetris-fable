module App

open Elmish
open Elmish.React
open Feliz

type State = { Count: int }

type Msg =
    | Increment
    | Decrement

let init () = { Count = 0 }

let update (msg: Msg) (state: State): State =
    match msg with
    | Increment -> { state with Count = state.Count + 1 }
    | Decrement -> { state with Count = state.Count - 1 }

[<Literal>]
let canvasWidth = 800

[<Literal>]
let canvasHeight = 800

let view (state: State) (dispatch: Msg -> unit) =
    let height = 10 * state.Count
    let width = 20 * state.Count
    Html.div
        [ Html.button
            [ prop.onClick (fun _ -> dispatch Increment)
              prop.text "Increment" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch Decrement)
                prop.text "Decrement" ]
          Html.h1 state.Count
          Html.svg
              [ prop.viewBox (0, 0, canvasWidth, canvasHeight)
                prop.children
                    [ Html.rect
                        [ prop.width canvasWidth
                          prop.height canvasHeight
                          prop.x 0
                          prop.y 0
                          prop.style [ style.fill "Orange" ]
                          prop.stroke "Red"
                          prop.strokeWidth 2 ]
                      Html.rect
                          [ prop.width width
                            prop.height height
                            prop.x 10
                            prop.y 10
                            prop.style [ style.fill "Black" ]
                            prop.stroke "Yellow"
                            prop.strokeWidth 2 ] ]
                unbox ("width", "40%") ] ]

Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run

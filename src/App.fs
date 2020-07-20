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

let view (state: State) (dispatch: Msg -> unit) =
    Html.div
        [ Html.button
            [ prop.onClick (fun _ -> dispatch Increment)
              prop.text "Increment" ]

          Html.button
              [ prop.onClick (fun _ -> dispatch Decrement)
                prop.text "Decrement" ]
          Html.h1 state.Count
          Html.svg
              [ //prop.viewBox (0, 0, 100, 100)
                Html.rect
                    [ prop.width 100
                      prop.height 200
                      prop.x 10
                      prop.y 20
                      prop.style [ style.fill "Orange" ]
                      prop.stroke "Red"
                      prop.strokeWidth 2 ]
                unbox ("width", "40%") ] ]

Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run

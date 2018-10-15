open Spotlib.Spot

type state = Game.state

let initial_state = Game.init

let from_string s = Game.of_string s

let step text prev =
  let open Result.Infix in
  Sashite.of_string text >>= fun sashite ->
  Game.next sashite prev

let to_string state = Game.to_string state

let to_error_message exn = Printexc.to_string exn

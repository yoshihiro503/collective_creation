open Util

type t = Sente | Gote

let initial_palyer = Sente

let next = function
  | Sente -> Gote
  | Gote -> Sente

let to_string = function
  | Sente -> "先手"
  | Gote -> "後手"
       
let of_string = function
  | "先手" -> `Ok Sente
  | "後手" -> `Ok Gote
  | other -> `Error (Failure (!%"Player.of_string: %s" other))

module List = Spotlib.Spot.List
open List.Infix
open Util

type t = int * int
let to_string (i, j) = !%"(%d,%d)" i j
let all : t list =
  (1 -- 9) |> List.map (fun dan ->
  (1 -- 9) |> List.map (fun suji ->
  (dan, suji))) |> List.flatten

let is_in_the_ban (i, j) =
  1 <= i && i <= 9
  && 1 <= j && j <= 9

let (+) (x, y) (i, j) = (x + i, y + j)
let (-) (x, y) (i, j) = (x - i, y - j)

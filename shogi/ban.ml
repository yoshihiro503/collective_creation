module List = Spotlib.Spot.List
open List.Infix
module Opt = Spotlib.Spot.Option
open Util

type nari = bool
type ('a, 'b) map = ('a * 'b) list
type t = (Position.t, Player.t * Koma.typ * nari) map

let empty : t = []
let add key koma_state t =
  (key, koma_state) :: t

let all_komas t =
  t

let init : t =
  empty
  |> add (1, 7) (Player.Sente, Koma.Hu, false)
  |> add (2, 7) (Player.Sente, Koma.Hu, false)
  |> add (3, 7) (Player.Sente, Koma.Hu, false)
  |> add (4, 7) (Player.Sente, Koma.Hu, false)
  |> add (5, 7) (Player.Sente, Koma.Hu, false)
  |> add (6, 7) (Player.Sente, Koma.Hu, false)
  |> add (7, 7) (Player.Sente, Koma.Hu, false)
  |> add (8, 7) (Player.Sente, Koma.Hu, false)
  |> add (9, 7) (Player.Sente, Koma.Hu, false)
  |> add (1, 9) (Player.Sente, Koma.Kyosha, false)
  |> add (2, 9) (Player.Sente, Koma.Keima, false)
  |> add (3, 9) (Player.Sente, Koma.Gin, false)
  |> add (4, 9) (Player.Sente, Koma.Kin, false)
  |> add (5, 9) (Player.Sente, Koma.OuT, false)
  |> add (6, 9) (Player.Sente, Koma.Kin, false)
  |> add (7, 9) (Player.Sente, Koma.Gin, false)
  |> add (8, 9) (Player.Sente, Koma.Keima, false)
  |> add (9, 9) (Player.Sente, Koma.Kyosha, false)
  |> add (2, 8) (Player.Sente, Koma.HisyaT, false)
  |> add (8, 8) (Player.Sente, Koma.KakuT, false)

  |> add (1, 3) (Player.Gote, Koma.Hu, false)
  |> add (2, 3) (Player.Gote, Koma.Hu, false)
  |> add (3, 3) (Player.Gote, Koma.Hu, false)
  |> add (4, 3) (Player.Gote, Koma.Hu, false)
  |> add (5, 3) (Player.Gote, Koma.Hu, false)
  |> add (6, 3) (Player.Gote, Koma.Hu, false)
  |> add (7, 3) (Player.Gote, Koma.Hu, false)
  |> add (8, 3) (Player.Gote, Koma.Hu, false)
  |> add (9, 3) (Player.Gote, Koma.Hu, false)
  |> add (1, 1) (Player.Gote, Koma.Kyosha, false)
  |> add (2, 1) (Player.Gote, Koma.Keima, false)
  |> add (3, 1) (Player.Gote, Koma.Gin, false)
  |> add (4, 1) (Player.Gote, Koma.Kin, false)
  |> add (5, 1) (Player.Gote, Koma.OuT, false)
  |> add (6, 1) (Player.Gote, Koma.Kin, false)
  |> add (7, 1) (Player.Gote, Koma.Gin, false)
  |> add (8, 1) (Player.Gote, Koma.Keima, false)
  |> add (9, 1) (Player.Gote, Koma.Kyosha, false)
  |> add (8, 2) (Player.Gote, Koma.HisyaT, false)
  |> add (2, 2) (Player.Gote, Koma.KakuT, false)
  
let find f (t: t) =
  List.find_opt f t
    
let find_key key t = find (fun (key', _) -> key = key') t |> Opt.fmap snd

let filter_map f (t: t) = List.filter_map f t

let remove pos t = List.filter(fun (pos', _) -> pos<>pos') t

let can_go ban pos (start, (player, koma, nari)) =
  match koma with
  | Koma.Hu | Keima | Gin | Kin | OuT ->
     Koma.nexts_relative player koma nari
     |> List.mem Position.(pos - start)
  | Kyosha when nari = true ->
     Koma.nexts_relative player koma nari
     |> List.mem Position.(pos - start)
  | Kyosha -> (* when nari = false *)
     let (dx, dy) = Position.(pos - start) in
     if dx = 0 && dy > 0 then (* 縦かつ前方方向 *)
       (1 -- (dy - 1))
       |> List.for_all (fun i -> (*移動空間は空白*)
              let pos' = Position.(start + (0, i)) in
              find_key pos' ban = None)
     else
       false
  | HisyaT ->
     let (dx, dy) = Position.(pos - start) in
     if dx = 0 then (*縦の移動*)
       (1 -- (abs dy - 1))
       |> List.for_all (fun i -> (*移動空間は空白*)
              let pos' = Position.(start + (0, i*dy/ abs dy)) in
              find_key pos' ban = None)
     else if dy = 0 then (*真横の移動*)
       (1 -- (abs dx - 1))
       |> List.for_all (fun i -> (*移動空間は空白*)
              let pos' = Position.(start + (i*dx/ abs dx, 0))in
              find_key pos' ban = None)
     else
       Koma.nexts_relative player Koma.OuT false (*成っているときは王の動きも*)
       |> List.mem Position.(pos - start)
  | KakuT ->
     let (dx, dy) = Position.(pos - start) in
     if abs dx = abs dy then (*斜め45度の位置にある*)
       (1 -- (abs dx - 1))
       |> List.for_all (fun i -> (*移動空間は空白*)
              let pos' = Position.(start + (i*dx / abs dx, i*dy / abs dy)) in
              find_key pos' ban = None)
     else
       Koma.nexts_relative player Koma.OuT false (*成っているときは王の動きも*)
       |> List.mem Position.(pos - start)

let can_go ban pos (start, (player, koma, nari)) =
  let y = can_go ban pos (start, (player, koma, nari)) in
  print_endline (!%"can_go: %s %s -> %s: %b" (Koma.typ_to_literal Player.Sente false koma) (Position.to_string start) (Position.to_string pos) y);
  y

let show_stage t : string =
  (1 -- 9) |> List.map (fun dan ->
    (1 -- 9) |> List.rev |> List.map (fun suji ->
      begin match find_key (suji, dan) t with
      | Some (player, koma, nari) ->
         Koma.typ_to_literal player nari koma
      | None -> if dan = 3 || dan = 6 then "＿" else "　"
      end ^ if suji = 7 || suji = 4 then "|" else "")
    |> String.concat "")
  |> String.concat "\n"
let to_string t = show_stage t
  (*
let show_mochigoma player t =
  let ms = mochigomas player t in
  let types = List.map Koma.to_typ ms |> list_nub in
  types
  |> List.map (fun ty ->
         (ty, List.filter (fun m -> Koma.to_typ m = ty) ms |> List.length))
  |> List.map (fun (ty, n) ->
         !%"%s%d" (Koma.typ_to_sente_ord_literal ty) n)
  |> String.concat ""
let to_string t =
  !%"%s\n\n先:%s\n後:%s"
    (show_stage t)
   *)

(* === parsing === *)
let parse_stage s =
  let rows = Str.split (Str.regexp_string "\n") s in
  (9, empty)
  |> List.fold_right
       (fun row (dan, stage) ->
         let cs =
           Utf8.of_string row |> Utf8.to_list |> List.filter ((<>) (Utf8.uchar_of_string "|"))
         in
         let komas =
           list_filter_mapi (fun j uchar ->
               let suji = 9 - j in
               match Koma.parse_uchar uchar with
               | `Ok (typ, nari, player) ->
                  Some ((suji, dan), typ, nari, player)
               | `Error _ ->
                  None) cs
         in
         stage |> List.fold_right (fun (pos, typ, nari, player) stage ->
                      add pos (player, typ, nari) stage) komas
         |> fun stage' -> (dan-1, stage')
       ) rows
  |> snd |> Result.return

let of_string s = parse_stage s
                (*
let of_string s : (t, exn) Result.t =
  let rows : string list = Str.split (Str.regexp_string "\n") s in
  let s_stage = List.take 9 rows |> String.concat "\n" in
  match List.drop 9 rows with
  | [""; s_mochi1; s_mochi2] ->
     print_endline (!%"stage: '%s'" s_stage);
     print_endline (!%"mochi1: '%s'" s_mochi1);
     print_endline (!%"mochi2: '%s'" s_mochi2);
     parse_stage s_stage
  | _ ->
     failwith "Ban.of_string"
                 *)

let is_nari_area player (_i, j) =
  match player with
  | Player.Sente ->
     1 <= j && j <= 3
  | Player.Gote ->
     7 <= j && j <= 9

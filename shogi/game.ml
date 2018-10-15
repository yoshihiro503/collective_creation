open Spotlib.Spot
module Opt = Spotlib.Spot.Option
open Util

type mochigoma =
  Koma.typ list

type state =
  Player.t * Ban.t * mochigoma * mochigoma

let init = (Player.Sente, Ban.init, [], [])

let player (player, _, _, _) = player
let ban (_, ban, _, _) = ban
let mochigoma_sente (_, _, m, _) = m
let mochigoma_gote  (_, _, _, m) = m
let get_mochigoma player state =
  match player with
  | Player.Sente -> mochigoma_sente state
  | Player.Gote  -> mochigoma_gote state
let set_mochigoma player ms (p, ban, m1, m2) =
  match player with
  | Player.Sente -> (p, ban, ms, m2)
  | Player.Gote  -> (p, ban, m1, ms)
let set_ban ban (p, _, m1, m2) = (p, ban, m1, m2)
let set_player player (_, ban, m1, m2) = (player, ban, m1, m2)
let add_mochigoma player koma (p, ban, m1, m2) =
  match player with
  | Player.Sente -> (p, ban, koma::m1, m2)
  | Player.Gote  -> (p, ban, m1, koma::m2)

let eq_state (s1: state) s2 =
  let eq_list xs ys = (List.sort compare xs = List.sort compare ys) in
  player s1 = player s2
  && eq_list (ban s1) (ban s2)
  && eq_list (mochigoma_sente s1) (mochigoma_sente s2)
  && eq_list (mochigoma_gote s1)  (mochigoma_gote s2)

let check_game_over state =
  if List.mem Koma.OuT (mochigoma_sente state) then Some Player.Sente
  else if List.mem Koma.OuT (mochigoma_gote state) then Some Player.Gote
  else None

let get_sashisaki sashite =
  match sashite.Sashite.position with
  | Pos(x, y) -> (x, y)
  | Dou -> failwith "unsupported: (同)"

(* 盤の中のコマを動かす *)
let next_from_ban state sashite : (state, exn) Result.t =
  let open Result.Infix in
  let player = player state in
  let sashisaki = get_sashisaki sashite in
  let (koma, _nari) = sashite.Sashite.koma in
  let naru = sashite.Sashite.nari in
  result_guard(sashite.Sashite.uchi = None) >>= fun () ->
  match
    Ban.find (begin fun (pos, (p, k, nari)) ->
              p = player && k = koma
              && Ban.can_go (ban state) sashisaki (pos, (player, koma, nari))
              end) (ban state)
  with
  | Some (pos, (player, koma, prev_nari)) ->
     begin match Ban.find_key sashisaki (ban state) with
     | Some (p, enemy_koma, _nari) when player <> p ->
         state
         |> set_ban (Ban.remove sashisaki (ban state))
         |> add_mochigoma player enemy_koma
         |> ok
     | None ->
        `Ok state
     | Some (_p, my_koma, k_nari) ->
        `Error (Failure (!%"すでに自軍の駒('%s')がいます" (Koma.typ_to_literal player k_nari my_koma)))
     end >>= fun state' ->
     (* 成と言っているのに成のエリアにいない場合は不可 *)
     result_guard(naru<>Some true || Ban.is_nari_area player sashisaki)>>= fun () ->
     let ban' =
       let next_nari = if naru = Some true then true else prev_nari in
       Ban.remove pos (ban state')
       |> Ban.add sashisaki (player, koma, next_nari)
     in
     set_ban ban' state'
     |> set_player (Player.next player)
     |> ok

  | None ->
     `Error(Failure (!%"動かせる候補がいません"))
    
let rec remove_mochigoma koma ms = (*対象の持ち駒を一つだけ減らす*)
  match ms with
  | m :: ms when m = koma -> ms
  | m :: ms' -> m :: remove_mochigoma koma ms'
  | [] -> failwith (!%"remove_mochigoma: %s <- [%s]" (Koma.show_typ koma) (slist "" Koma.show_typ ms))

let next_from_mochigoma state sashite : (state, exn) Result.t =
  let open Result.Infix in
  let player = player state in
  let sashisaki = get_sashisaki sashite in
  let ms = get_mochigoma player state in
  let (koma, _nari) = sashite.Sashite.koma in
  (* 指すコマを持ち駒として保持していて、かつ現段階で指し先の場所が空だったら成功する *)
  (result_guard (List.mem koma ms) @? (!%"持っていない駒(%s)を打てません" (Koma.show_typ koma))) >>= fun () ->
  match Ban.find_key sashisaki (ban state) with
  | None ->
     let ms' = remove_mochigoma koma ms in
     let nari = false in
     let ban' = Ban.add sashisaki (player, koma, nari) (ban state) in
     let state' =
       set_mochigoma player ms' state
       |> set_ban ban'
       |> set_player (Player.next player)
     in
     `Ok state'
  | Some (player, koma, nari) ->
     `Error (Failure (!%"コマがあるところに打てません: %s" (Koma.typ_to_literal player nari koma)))

let next sashite (state : state) =
  result_or
    (next_from_ban state sashite)
    (next_from_mochigoma state sashite)

let to_string t =
  let ban = ban t in
  !%"%s\n%s\n\n先:%s\n後:%s" (Player.to_string (player t)) (Ban.to_string ban)
    (slist "" Koma.show_typ (mochigoma_sente t))
    (slist "" Koma.show_typ (mochigoma_gote t))

let rec many (p: 'cs -> ('a*'cs, exn) Result.t)  cs : ('a list * 'cs, exn) Result.t =
  let open Result.Infix in
  result_or
    begin
      p cs >>= fun (x, cs') ->
      many p cs' >>= fun (xs, cs'') ->
      `Ok(x::xs, cs'')
    end
    (`Ok([], cs))

let mochigoma_of_string s : (mochigoma, exn) Result.t =
  let open Result.Infix in
  let cs = Utf8.of_string s |> Utf8.to_list |> List.drop 2 in
  let is_digit c =
    match Utf8.uchar_to_string c with
    | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> true
    | _ -> false
  in
  let parse_digit = function
    | [] -> `Error (Failure"_")
    | c :: cs when is_digit c -> `Ok(c, cs)
    | _ -> `Error(Failure"_")
  in
  let parse_digits cs = many parse_digit cs in
  let parse_num cs =
    match parse_digits cs with
    | `Ok([], _) -> `Error(Failure "parse_num")
    | `Ok(digits, cs) ->
       let num = Utf8.of_list digits |> Utf8.to_string |> int_of_string in
       `Ok(num, cs)
    | _ -> `Error (Failure"must not be here")
  in
  let parse1 = function
    | [] -> `Error (Failure(!%"mochigoma_of_string(%s): parse1" s))
    | c :: cs ->
       Koma.parse_uchar c >>= fun (koma,_,_) ->
       begin match parse_num cs with
       | `Ok (n, cs') ->
          `Ok(List.make n koma, cs')
       | `Error _ ->
          `Ok([koma], cs)
       end
  in
  match many parse1 cs with
  | `Ok (komass, []) -> `Ok(List.concat komass)
  | _ -> `Error(Failure(!%"mochigoma_of_string: '%s'" s))

let of_string s : (state, exn) Result.t =
  let ([s], ss) = Str.split (Str.regexp_string "\n") s |> List.split_at 1 in
  let s_next_player = s in
  let (ss1, ss2) = List.split_at 9 ss in
  let s_ban = String.concat "\n" ss1 in
  let open Result.Infix in
  Player.of_string s_next_player >>= fun next_player ->
  Ban.of_string s_ban >>= fun ban ->
  begin match List.tl ss2 with
  | [s_sente; s_gote] ->
     mochigoma_of_string s_sente >>= fun mochi_sente ->
     mochigoma_of_string s_gote >>= fun mochi_gote ->
     `Ok(next_player, ban, mochi_sente, mochi_gote)
  | _ -> `Error(Failure (!%"of_string: mochigoma: %s" (String.concat "[\\n]" ss2)))
  end

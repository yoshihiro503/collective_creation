open Util

       (*
type t =
  | Hu1
  | Hu2
  | Hu3
  | Hu4
  | Hu5
  | Hu6
  | Hu7
  | Hu8
  | Hu9
  | Kyosha1
  | Kyosha2
  | Keima1
  | Keima2
  | Gin1
  | Gin2
  | Kin1
  | Kin2
  | Hisya
  | Kaku
  | Ou
        *)
type typ =
  | Hu
  | Kyosha
  | Keima
  | Gin
  | Kin
  | HisyaT
  | KakuT
  | OuT

(* 次行ける相対位置を規定 (香車、飛車、角以外のコマで有効) *)
let nexts_gote_ord_relative = function
  | Hu -> [(0, 1)]
  | Keima -> [(-1, 2); (1, 2)]
  | Gin -> [(0, 1); (1, 1); (-1, 1); (-1, -1); (1, -1)]
  | Kin -> [(0, 1); (1, 1); (-1, 1); (-1, 0); (1, 0); (0, -1)]
  | OuT -> [(0, 1); (1, 1); (-1, 1); (-1, 0); (1, 0); (-1, -1); (0, -1); (1, -1)]
  | Kyosha | HisyaT | KakuT -> failwith "nexts"
let nexts_gote_nari_relative = function
  | Hu | Keima | Gin | Kin ->
     nexts_gote_ord_relative Kin
  | OuT -> nexts_gote_ord_relative OuT
  | Kyosha | HisyaT | KakuT -> failwith "nexts"
let nexts_gote_relative typ nari =
  if nari then nexts_gote_nari_relative typ
  else nexts_gote_ord_relative typ
let nexts_sente_relative typ nari =
  nexts_gote_relative typ nari |> List.map (fun (i, j) -> (i, -j))
let nexts_relative player koma nari =
  match player with
  | Player.Sente ->
     nexts_sente_relative koma nari
  | Player.Gote ->
     nexts_gote_relative koma nari

let typ_to_sente_ord_literal = function
  | Hu -> "歩"
  | Kyosha ->"香"
  | Keima -> "桂"
  | Gin -> "銀"
  | Kin -> "金"
  | HisyaT -> "飛"
  | KakuT -> "角"
  | OuT -> "玉"
let show_typ = typ_to_sente_ord_literal

let typ_to_gote_ord_literal = function
  | Hu -> "步"
  | Kyosha ->"⾹"
  | Keima -> "挂"
  | Gin -> "银"
  | Kin -> "⾦"
  | HisyaT -> "⾶"
  | KakuT -> "⻆"
  | OuT -> "⽟"

let typ_to_gote_ord_literal' = function
  | Hu -> "フ"
  | Kyosha ->"ヨ"
  | Keima -> "ケ"
  | Gin -> "ギ"
  | Kin -> "キ"
  | HisyaT -> "ヒ"
  | KakuT -> "カ"
  | OuT -> "オ"

let typ_to_sente_nari_literal = function
  | Hu -> "と"
  | Kyosha ->"仝"
  | Keima -> "今"
  | Gin -> "全"
  | Kin -> "き"
  | HisyaT -> "竜"
  | KakuT -> "馬"
  | OuT -> "将"

let typ_to_gote_nari_literal = function
  | Hu -> "ト"
  | Kyosha ->"㒰"
  | Keima -> "𫝆"
  | Gin -> "全"
  | Kin -> "－"
  | HisyaT -> "⻯"
  | KakuT -> "⾺"
  | OuT -> "－"

let typ_to_gote_nari_literal' = function
  | Hu -> "ト"
  | Kyosha ->"ド"
  | Keima -> "イ"
  | Gin -> "ゼ"
  | Kin -> "－"
  | HisyaT -> "リ"
  | KakuT -> "ウ"
  | OuT -> "－"

let typ_to_literal player nari typ =
  match player with
  | Player.Sente when nari ->
     typ_to_sente_nari_literal typ
  | Player.Sente ->
     typ_to_sente_ord_literal typ
  | Player.Gote  when nari ->
     typ_to_gote_nari_literal typ
  | Player.Gote  ->
     typ_to_gote_ord_literal typ

(* === parsing === *)
let parse_uchar c =
  match Utf8.uchar_to_string c with
  | "歩" -> `Ok (Hu, false, Player.Sente)
  | "香" -> `Ok (Kyosha, false, Player.Sente)
  | "桂" -> `Ok (Keima, false, Player.Sente)
  | "銀" -> `Ok (Gin, false, Player.Sente)
  | "金" -> `Ok (Kin, false, Player.Sente)
  | "飛" -> `Ok (HisyaT, false, Player.Sente)
  | "角" -> `Ok (KakuT, false, Player.Sente)
  | "玉" -> `Ok (OuT, false, Player.Sente)
  | "步" -> `Ok (Hu, false, Player.Gote)
  | "⾹" -> `Ok (Kyosha, false, Player.Gote)
  | "挂" -> `Ok (Keima, false, Player.Gote)
  | "银" -> `Ok (Gin, false, Player.Gote)
  | "⾦" -> `Ok (Kin, false, Player.Gote)
  | "⾶" -> `Ok (HisyaT, false, Player.Gote)
  | "⻆" -> `Ok (KakuT, false, Player.Gote)
  | "⽟" -> `Ok (OuT, false, Player.Gote)
  | "と" -> `Ok (Hu, true, Player.Sente)
  | "仝" -> `Ok (Kyosha, true, Player.Sente)
  | "今" -> `Ok (Keima, true, Player.Sente)
  | "全" -> `Ok (Gin, true, Player.Sente)
  | "竜" -> `Ok (HisyaT, true, Player.Sente)
  | "馬" -> `Ok (KakuT, true, Player.Sente)
  | "ト" -> `Ok (Hu, true, Player.Gote)
  | "㒰" -> `Ok (Kyosha, true, Player.Gote)
  | "𫝆" -> `Ok (Keima, true, Player.Gote)
  | "全" -> `Ok (Gin, true, Player.Gote)
  | "⻯" -> `Ok (HisyaT, true, Player.Gote)
  | "⾺" -> `Ok (KakuT, true, Player.Gote)
  | s ->
     `Error (Failure ("Koma.parse_uchar: " ^ s))

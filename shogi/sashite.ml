open Util
open Spotlib.Spot
open Spotlib.Spot.Result.Infix

type modifier =
  | Left
  | Right
  | Straight (*直*)
  | Yori
  | Up
  | Down

type pos = Pos of Position.t | Dou
type nari = bool
type koma = Koma.typ * nari
type t = {
    position : pos;
    koma : koma;
    modifiers : modifier list;
    nari : bool option; (* 成or不成の明示指定があるなし *)
    uchi : unit option;
  }

let string_of_modifier = function
  | Left     -> "左"
  | Right    -> "右"
  | Straight -> "直"
  | Yori -> "寄"
  | Up   -> "上"
  | Down -> "下"
let to_string t =
  let pos = match t.position with Pos(i,j) -> !%"(%d,%d)" i j | Dou -> "同" in
  let (koma, _) = t.koma in
  let nari = match t.nari with None -> "-" | Some true -> "yes" | Some false -> "no" in
  let mods = slist "," string_of_modifier t.modifiers in
  !%"Sashite(%s,%s,nari:%s,[%s])" pos (Koma.typ_to_literal Player.Sente false koma) nari mods

let read_utf8_digit c =
  let s = Utf8.string_of_uchar c in
  let zenkaku =
    match s with
    | "１" -> `Ok 1
    | "２" -> `Ok 2
    | "３" -> `Ok 3
    | "４" -> `Ok 4
    | "５" -> `Ok 5
    | "６" -> `Ok 6
    | "７" -> `Ok 7
    | "８" -> `Ok 8
    | "９" -> `Ok 9
    | _ -> `Error (Failure "zenkaku")
  in
  let kansuji =
    match s with
    | "一" -> `Ok 1
    | "二" -> `Ok 2
    | "三" -> `Ok 3
    | "四" -> `Ok 4
    | "五" -> `Ok 5
    | "六" -> `Ok 6
    | "七" -> `Ok 7
    | "八" -> `Ok 8
    | "九" -> `Ok 9
    | _ -> `Error (Failure "kansuji")
  in
  result_or zenkaku kansuji
let read_digit c =
  let s = Utf8.string_of_uchar c in
  result_or
    (Result.catch_exn (fun () -> int_of_string s))
    (read_utf8_digit c)
  >>|! (fun _ -> Failure (!%"read_digit: '%s'" s))
  
let dou = "同" |> Utf8.uchar_of_string
let parse_pos = function
  | c :: cs when c = dou -> `Ok (Dou, cs)
  | c1 :: c2 :: cs ->
     read_digit c1 >>= fun dan ->
     read_digit c2 >>= fun suji ->
     `Ok (Pos(dan, suji), cs)
  | _ -> `Error (Failure"parse_pos")

let parse_koma = function
  | [] -> `Error (Failure "parse_koma: empty")
  | c :: cs ->
     Koma.parse_uchar c >>| fun (koma,nari,_) -> ((koma, nari), cs)

let parse_modifier1 = function
  | [] -> `Error (Failure "")
  | c :: cs ->
     begin match Utf8.uchar_to_string c with
     | "左" -> `Ok (Left, cs)
     | "右" -> `Ok (Right, cs)
     | "直" -> `Ok (Straight, cs)
     | _ -> `Error (Failure "")
     end
let parse_modifier2 = function
  | [] -> `Error (Failure "")
  | c :: cs ->
     begin match Utf8.uchar_to_string c with
     | "寄" -> `Ok (Yori, cs)
     | "上" -> `Ok (Up, cs)
     | "下" -> `Ok (Down, cs)
     | _ -> `Error (Failure "")
     end
let parse_nari_uchi = function
  | [] -> `Error (Failure "")
  | c :: cs ->
     begin match Utf8.uchar_to_string c with
     | "打" -> `Ok (`Uchi, cs)
     | "成" -> `Ok (`Nari, cs)
     | "不" -> `Ok (`FuNari, cs) (*どうせこれより先は見ないからこれでいい*)
     | _ -> `Error (Failure "")
     end

let opt p cs =
  result_or (p cs >>| fun (x, cs') -> (Some x, cs')) (`Ok (None, cs))

let parse cs =
  begin
    parse_pos cs >>= fun (position, cs') ->
    parse_koma cs' >>= fun (koma, cs'') ->
    opt parse_modifier1 cs''  >>= fun (mod1, cs''') ->
    opt parse_modifier2 cs''' >>= fun (mod2, cs'''') ->
    let modifiers = List.filter_map id [mod1; mod2] in
    opt parse_nari_uchi cs'''' >>= fun (nu, cs) ->
    let (uchi, nari) = match nu with
      | Some `Uchi -> (Some(), None)
      | Some `Nari -> (None, Some true)
      | Some `FuNari -> (None, Some false)
      | _ -> (None, None)
    in
    `Ok ({position; koma; modifiers; nari; uchi}, cs)
  end
  >>|! (fun exn -> Failure (!%"parse: '%s' :%s" (Utf8.of_list cs |> Utf8.to_string) (exn_message exn)))

let of_string s =
  let cs = s |> Utf8.of_string |> Utf8.to_list in
  match parse cs with
  | `Ok(t, _) -> `Ok t
  | `Error e -> `Error e

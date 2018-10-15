open Util

let get = function
  | `Ok x -> x
  | `Error (Failure msg) ->
     prerr_endline msg;
     failwith msg
  | `Error exn -> raise exn

let check state =
  let s = Game.to_string state in
  match Game.of_string s with
  | `Ok state' when Game.eq_state state state' ->
     ()
  | `Ok state' ->
     print_endline(!%"state:\n%s"  (Game.to_string state));
     print_endline(!%"state':\n%s" (Game.to_string state'));
     failwith (!%"is not eq")
  | `Error exn ->
     raise exn
    
let () = (*to_string, of_string*)
  let init = Game.init in
  check init

let () =
  let sashites = [(* https://shogidb2.com/games/e744ff1dbb3d13383298501a810a94acd521774c#lnsgkgsnl%2F1r5b1%2Fppppppppp%2F9%2F9%2F2P6%2FPP1PPPPPP%2F1B5R1%2FLNSGKGSNL%20w%20-%202 *)
      "７六歩";
      "８四歩";
      "６八銀";
      "３四歩";
      "７七銀";
      "６二銀";
      "２六歩";
      "４二銀";
      "２五歩";
      "３三銀";
      "５六歩";
      "３二金";
      "４八銀";
      "５二金";
      "７八金";
      "５四歩";
      "３六歩";
      "４一玉";
      "４六歩";
      "３一角";
      "３七桂";
      "４四歩";
      "４七銀";
      "８五歩";
      "２九飛";
      "４三金右";
      "４八玉";
      "１四歩";
      "１六歩";
      "５三銀";
      "６六銀";
      "６四銀";
      "３八玉";
      "８六歩";
      "86歩";
      "86飛";
      "４八金";
      "４二角";"９六歩";"８二飛";"８七歩";"３一玉";"５五歩";
      "55歩";"55銀";"55銀";"55角";"５六歩";"56銀";"５二飛";"５七歩";"３五歩";"35歩";"３六歩";"８八角";"７四歩";"４七金";"３七歩成";
      "37玉";"７五歩";"１五歩";"15歩";"１三歩";"５四銀";"３六金";
      "１三香";"１二歩";"２二銀";"５五銀打";"１六歩";
      "４四銀";"１七歩成";"４七玉";"５五歩";"４三銀成";"43銀";"５五角";"４四桂";"９一角成";"３六桂";
      "５八玉";"２八と";"７九飛";"７六歩";"６六歩";"１九香成";"８一馬";"８八歩";"４四桂";"44銀";

      "６三馬";"４一銀";"３六馬";"８九歩成";"89飛";"５六飛";"56歩";"６五桂";"65歩";"６六桂";
      "５七玉";"７八桂成";"３四桂";"７五角";"６六香";"４八銀";"48玉";"６六角";"４七玉";"４八金";
      "３七玉";"３八金";"２六玉";"４八角成";
    ]
    |> List.map (get <<< Sashite.of_string)
  in
  let state = Game.init in
  print_endline (Game.to_string state);
  sashites
  |> List.fold_left (fun (i, state) sashite ->
         begin match Game.next sashite state with
         | `Ok state' ->
            print_endline (!%"=== %d: %s" i (Sashite.to_string sashite));
            print_endline (!%"%s" (Game.to_string state'));
            check state';
            (i+1, state')
         | `Error (Failure msg) ->
            prerr_endline msg;
            prerr_endline (!%"no available te: %s" (Sashite.to_string sashite));
            failwith msg
         | `Error e ->
            raise e
         end) (1, state)
  |> ignore

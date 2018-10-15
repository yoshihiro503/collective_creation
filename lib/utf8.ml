let string_of_chars cs =
  cs |> List.map (String.make 1) |> String.concat ""
  
type uchar =
  | Ascii of char (*7bit*)
  | C2 of char*char (*2バイト*)
  | C3 of char*char*char (*3バイト*)
  | C4 of char*char*char*char (*4バイト*)
  | C5 of char*char*char*char*char (*5バイト*)
  | C6 of char*char*char*char*char*char (*6バイト*)

type utf8 = uchar list

type t = utf8

type bits = bool*bool*bool*bool * bool*bool*bool*bool
let bits_of_char char =
  let n = int_of_char char in
  (0b1000_0000 land n <> 0
  , 0b0100_0000 land n <> 0
  , 0b0010_0000 land n <> 0
  , 0b0001_0000 land n <> 0
  , 0b0000_1000 land n <> 0
  , 0b0000_0100 land n <> 0
  , 0b0000_0010 land n <> 0
  , 0b0000_0001 land n <> 0
  )

let of_string s =
  let n = String.length s in
  let rec iter store i =
    if i >= n then (List.rev store)
    else
      let c1 = s.[i] in
      match bits_of_char c1 with
      | (false,_,_,_, _,_,_,_) -> iter (Ascii c1::store) (i+1)
      | (true,true,false,_, _,_,_,_) ->
          iter (C2(c1,s.[i+1])::store) (i+2)
      | (true,true,true,false, _,_,_,_) ->
          iter (C3(c1,s.[i+1],s.[i+2])::store) (i+3)
      | (true,true,true,true, false,_,_,_) ->
          iter (C4(c1,s.[i+1],s.[i+2],s.[i+3])::store) (i+4)
      | (true,true,true,true, true,false,_,_) ->
          iter (C5(c1,s.[i+1],s.[i+2],s.[i+3],s.[i+4])::store) (i+5)
      | (true,true,true,true, true,true,false,_) ->
         iter (C6(c1,s.[i+1],s.[i+2],s.[i+3],s.[i+4],s.[i+5])::store) (i+6)
      | _ -> failwith "of_string"
  in
  iter [] 0

let uchar_of_string s =
  match of_string s with
  | [c] -> c
  | _ -> failwith ("Utf8.uchar_of_string: " ^ s)

let string_of_uchar = function
  | Ascii c -> String.make 1 c
  | C2 (a,b) -> string_of_chars [a;b]
  | C3 (a,b,c) -> string_of_chars [a;b;c]
  | C4 (a,b,c,d) -> string_of_chars [a;b;c;d]
  | C5 (a,b,c,d,e) -> string_of_chars [a;b;c;d;e]
  | C6 (a,b,c,d,e,f) -> string_of_chars [a;b;c;d;e;f]
let uchar_to_string = string_of_uchar

let is_ascii = function
  | Ascii _ -> true
  | _ -> false

let to_string t =
  List.map uchar_to_string t
  |> String.concat "" 

let append t1 t2 = t1 @ t2

let length t = List.length t

let get t i = List.nth t i
let of_list cs = cs
let to_list t = t

let drop n t =
  let rec iter = function
    | (_, []) -> []
    | (0, cs) -> cs
    | (n, _c::cs) -> iter (n-1, cs)
  in
  iter (n, t)

let take n t =
  let rec iter store = function
    | (_, []) -> List.rev store
    | (0, _) -> List.rev store
    | (n, c::cs) -> iter (c::store) (n-1, cs)
  in
  iter [] (n, t)

let sub t i len = drop i t |> take len

module Op = struct
  let (^) = append
end

open Op

let summary maxlen t =
  if length t <= maxlen then t
  else sub t 0 (maxlen-3) ^ (of_string "...")

(*
let aiu = [
  C3('\xE3','\x81','\x82');
  C3('\xE3','\x81','\x84');
  C3('\xE3','\x81','\x86');
]

let check =
  let t = of_string "あいう" in
  t = aiu
*)

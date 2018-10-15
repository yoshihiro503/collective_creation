module Result = Spotlib.Spot.Result
module Stream = Spotlib.Spot.Stream
module List = Spotlib.Spot.List

let (!%) f = Printf.sprintf f
let (<<<) g f x = g (f x)
let id x = x
let slist delim f xs = String.concat delim (List.map f xs)
let sopt f = function
  | Some x -> f x
  | None -> "None"
let exn_message = function
  | Failure msg -> msg
  | exn -> Printexc.to_string exn

let rec list_nub_with f = function
  | [] -> []
  | x :: xs -> x :: list_nub_with f (List.filter (fun y -> f y <> f x) xs)

let list_nub xs = list_nub_with id xs

let list_filter_mapi f xs =
  List.mapi f xs |> List.filter_map id

let string_take k s =
  let n = String.length s in
  if k >= n then s
  else String.sub s 0 k

let string_take_right k s =
  let n = String.length s in
  if k >= n then s
  else String.sub s (n - k) k
let string_summarize k s =
  let n = String.length s in
  if k >= n then s
  else
    let dots = "..." in
    dots ^ string_take_right (k-3) s

let result_of_option = function
  | Some x -> `Ok x
  | None -> `Error (Failure "none")

let is_ok = function
  | `Ok _ -> true
  | `Error _ -> false

let result_guard cond =
  if cond then `Ok () else `Error (Failure "guard")
let (@?) result message =
  match result with
  | `Ok x -> `Ok x
  | `Error (Failure msg) ->
     `Error (Failure(message ^ ": " ^ msg))
  | `Error exn -> `Error (Failure(message ^ ": " ^ Printexc.to_string exn))
let result_or x y =
  match x with
  | `Ok x -> `Ok x
  | `Error (Failure msg) -> y @? msg
  | `Error _exn -> y

let from_Some = function
  | Some v -> v
  | _ -> assert false

let rec drop_while f xs =
  match xs with
  | lazy Stream.Null -> xs
  | lazy (Stream.Cons(x, xs')) ->
     if f x then drop_while f xs'
     else xs

(* 重複を除去する *)
let stream_nub_with f str =
  Stream.create (fun (knowns, xs) ->
      let is_known x = List.mem x knowns in
      match drop_while (is_known <<< f) xs with
      | lazy Stream.Null -> None
      | lazy (Cons(x, xs')) ->
         Some(x, (f x :: knowns, xs')))
    ([], str)

let stream_nub str = stream_nub_with id str

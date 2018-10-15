open OCamltter_twitter
module Stream =Spotlib.Spot.Stream
module Json = Tiny_json.Json
open Util
open Api_intf
open Api11
open Spotlib.Spot.Result.Infix

type error = Api11.Error.t

let string_of_err (err : error) =
  match err with
  | `Curl (_code, i, s) -> !%"Tw.error(curl): %d %s" i s
  | `Http (i, s) -> !%"Tw.error(http): %d '%s'" i s
  | `Json (_desc, json, _) -> !%"Tw.error(Json): %s" (Json.show json)
  | `Json_parse (e, s) ->
     !%"Tw.error(Json_parse): %s: '%s'" (Printexc.to_string e) s

let exn_of_err (err : error) =
  (Format.printf "%a@." Error.format) err;
  Failure (string_of_err err)


module ETweet = struct
  open Meta_conv.Open
  type t = <
    id        : int64;
    id_str    : string;
    user      : User.t;
    full_text      : Text.t;
    truncated : bool;

    in_reply_to_status_id   : int64 option; (* RE or RT *)
    in_reply_to_user_id     : int64 option;
    in_reply_to_screen_name : string option;
    retweeted_status        : t mc_option; (* RT *)

    created_at : Time.t;

    source : Client.t; (* html piece *)

    retweet_count : int;
    favorited     : bool;
    retweeted     : bool;


    entities : Entities.t mc_option;

    display_text_range : int list;
    in_reply_to_status_id_str: string option;
    in_reply_to_user_id_str: string option;
    geo : string option;
    coordinates : string option;
    place : string option;
    contributors : string option;
    is_quote_status : bool;
    favorite_count : int;
    lang : string;

  > [@@deriving conv{ocaml; json}]

  type ts = t list [@@deriving conv{ocaml; json}]

  let format    x = Ocaml.format_with ~no_poly:true ~raw_string:true ocaml_of_t x
  let format_ts x = Ocaml.format_with ~no_poly:true ~raw_string:true ocaml_of_ts x
end

type tweet = ETweet.t

let get_fulltext (tw : tweet) =
  tw#full_text

let format_tweet ppf t =
  Format.fprintf ppf "{ @[<v>user=%S; id=%LdL;@ text=\"%s\"@] }"
    (from_Some t#user#details)#screen_name
    t#id
    t#full_text

let print_tweets tws =
  List.iter (Format.printf "%a@." format_tweet) tws

let get_screen_name (tw: tweet) =
  match tw#user#details with
  | Some detail -> detail#screen_name
  | None -> "__unknown__"

let update = Tweets.update

let utf8_take_right k s =
  Utf8.drop (Utf8.length s - k) s
let utf8_summarize k s =
  let n = Utf8.length s in
  if k >= n then s
  else
    let _ =
      print_endline (!%"Twitter_util.utf8_summarize %d: '%s'" (k-3)
                       (utf8_take_right (k-3) s |> Utf8.to_string))
    in
    let dots = Utf8.of_string "..." in
    Utf8.append dots (utf8_take_right (k - Utf8.length dots) s)

let reply oauth tw s =
  print_endline (!%"repling... '%s'" s);
  let name = get_screen_name tw in
  let id = tw#id in
  (*文字列を140文字以内にする *)
  let text_body =
    let max = 140 - (String.length name + 2) in
    Utf8.of_string s |> utf8_summarize max |> Utf8.to_string
  in
  let text = (!%"@%s %s" name text_body) in
  print_endline (!%"reply: text: '%s'" text);
  Api11.Tweets.update ~in_reply_to_status_id:id oauth text

(* stream版 *)
  (*
let get_mentions oauth =
  Api11.Timelines.mentions_timeline_stream oauth
  |> Stream.filter_map (function
         | `Ok tw -> Some tw
         | `Error err ->
            print_endline (!%"warning: %s" (string_of_err err));
            None)
   *)

let show_with_tweet_mode =
  let open Spotlib.Spot in
  let open Arg in
  let tweet_mode k opts ?tweet_mode =
    optional_args k opts ["tweet_mode", of_string tweet_mode]
  in
  get ETweet.t_of_json "statuses/show/%Ld.json" 
    &  trim_user
    ** include_my_tweet
    ** include_entities
    ** tweet_mode

let show_with_extended oauth id =
  show_with_tweet_mode ~tweet_mode:"extended" oauth id
  >>|! exn_of_err
              
let mentions_timeline_with_tweet_mode =
  let open Spotlib.Spot in
  let open Arg in
  let tweet_mode k opts ?tweet_mode =
    optional_args k opts ["tweet_mode", of_string tweet_mode]
  in
  Arg.get ETweet.ts_of_json "statuses/mentions_timeline.json"
    &  count 
    ** since_max_ids
    ** trim_user
    ** contributor_details
    ** include_entities
    ** tweet_mode


let get_mentions_with_extended oauth : ETweet.t Stream.t =
  print_endline (!%"mentions_timeline_with_extended");
  match mentions_timeline_with_tweet_mode ~tweet_mode:"extended" oauth with
  | `Ok tws -> Stream.of_list tws
  | `Error err ->
     Format.printf "%a@." Error.format err;
     failwith "get_mentions"
     
let fav = Api11.Favorites.create

(* '@name hello' -> 'hello' *)
let remove_reply_name text =
  let regexp = Str.regexp "^@[a-zA-Z0-9_]+\ " in
  let text' = Str.global_replace regexp "" text in
  text'

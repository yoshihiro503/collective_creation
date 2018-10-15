open Spotlib.Spot
module Result = Spotlib.Spot.Result
open Result.Infix
open Util
module Tw = Twitter_util

module type Step = sig
  type state

  val initial_state : state
  val from_string : string -> (state, exn) Result.t
  val to_string : state -> string
  val step : string -> state -> (state, exn) Result.t
  val to_error_message : exn -> string
end

module Make(S : Step) = struct
  let my_name = "replyme_now"
(*
  (* my_nameで始まる最初のリプライ指定を削除 *)
  let remove_name text =
    let name_part = "@" ^ my_name ^ " " in
    let len = String.length name_part in
    if String.sub text 0 len = name_part then begin
        let text' = String.sub text len (String.length text - len) in
        print_endline (!%"remove_name: '%s' -> '%s'" text text');
        text'
    end else text
 *)

  let get_prev oauth (reply : Tw.tweet) =
    match reply#in_reply_to_status_id with
    | Some prev_id ->
       Tw.show_with_extended oauth prev_id >>= fun prev_tw ->
       let text = Tw.get_fulltext prev_tw |> Tw.remove_reply_name in
       S.from_string text
    | None ->
       `Ok S.initial_state

  let handle_reply oauth (reply : Tw.tweet) =
    get_prev oauth reply >>= fun prev ->
    let reply_text = Tw.remove_reply_name (Tw.get_fulltext reply) in
    (match S.step reply_text prev with
    | `Ok next ->
       (Tw.reply oauth reply (S.to_string next) >>|! Tw.exn_of_err) >>| fun rrr ->
       `Ok(`Success rrr)
    | `Error exn ->
       `Ok(`Invalid_arguments exn))

  (* 自分への新しいメンションを取得
   * favしたものは除外する(reply済みはfavっていくので)
   * 古い順にソートすることで、取り逃しを少なくする
   *)
  let find_new_replies oauth =
    Tw.get_mentions_with_extended oauth
    |> stream_nub_with (fun t -> t#id) (*どういうわけか重複が発生するので除去*)
    |> Stream.filter (fun tw -> not tw#favorited) (*fav済を除去*)
    |> Stream.filter
         (fun tw -> Tw.get_screen_name tw <> my_name) (*なぜか自分のが含まれるので除去*)
    |> Stream.take 10
    |> Stream.to_list
    |> List.rev

  let reply oauth tw =
    begin match handle_reply oauth tw with
    | `Ok (`Ok _) ->
       Tw.fav oauth tw#id |> ignore;
       print_endline "reply success"
    | `Ok (`Invalid_arguments exn) ->
       Tw.fav oauth tw#id |> ignore;
       print_endline (!%"invalid arguments: %s" (Printexc.to_string exn))
    | `Error exn ->
       let msg = match exn with
         | Failure m -> m
         | exn -> Printexc.to_string exn
       in
       Printexc.get_backtrace() |> prerr_endline;
       prerr_endline msg
    end

  let work oauth =
    begin match find_new_replies oauth with
    | [] ->
       print_endline "no new reply found"
    | ts ->
       print_endline (!%"%d new reply(ies) found" (List.length ts));
       ts |> Tw.print_tweets;
       ts |> List.iter (reply oauth)
    end
end

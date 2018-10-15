module F = Func.Make(Echo_state)

let auth_file = "bot_auth_replyme_now"

let () =
  let oauth = Oauth.Oauth.get auth_file in
  F.work oauth

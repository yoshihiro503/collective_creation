module F = Func.Make(Shogi_state)

let auth_file = "bot_auth_d0sas"

let () =
  let oauth = Oauth.Oauth.get auth_file in
  F.work oauth

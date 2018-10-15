open OCamltter_oauth    
module Oauth = struct
  include Oauth_ex.Make(struct
    include OCamltter_twitter.Conf
    let app = { Oauth_ex.Consumer.key = "vS0nKAS6ieWL76zZaQgF4A";
                secret = "XHa1ZiPcNRsYKw4mdIv8wHUiNulpBFxKT1ntXXuJgo"; }
  end)

  let load auth_file =
    match Ocaml.load_with_exn Access_token.t_of_ocaml auth_file with
    | [a] -> a
    | _ -> assert false
  
  let get_acc_token auth_file =
    try load auth_file with
    | _ -> 
        let _res, acc_token = authorize_cli_interactive () in
        Ocaml.save_with Access_token.ocaml_of_t ~perm:0o600 auth_file [acc_token];
        acc_token
  
  let get auth_file =
    let acc_token = get_acc_token auth_file in
    oauth Conf.app acc_token
end

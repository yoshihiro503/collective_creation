module Tw = Twitter_util

type state = string

let initial_state = ""

let from_string s = `Ok s

let step text prev = `Ok (prev ^ text)

let to_string s = s

let to_error_message exn = Printexc.to_string exn

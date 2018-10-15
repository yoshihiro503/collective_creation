type utf8
type t = utf8
type uchar
type bits
val of_string : string -> utf8
val uchar_of_string : string -> uchar
val to_string : utf8 -> string
val uchar_to_string : uchar -> string
val string_of_uchar : uchar -> string
val is_ascii : uchar -> bool
val append : t -> t -> t
val length : t -> int
val get : t -> int -> uchar
val to_list : t -> uchar list
val of_list : uchar list -> t
val drop : int -> t -> t
val take : int -> t -> t
val sub : t -> int -> int -> t
val summary : int -> t -> t

module Op : sig
  val (^) : t -> t -> t
end



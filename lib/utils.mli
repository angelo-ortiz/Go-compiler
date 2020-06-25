
type loc = Lexing.position * Lexing.position

module Smap : Map.S with type key = string
type 'a smap = 'a Smap.t

val red : string
val yellow : string
val blue : string
val invert : string
val close : string
val dummy_loc : Lexing.position * Lexing.position
val word_size : int

val position_of_loc : Lexing.position * Lexing.position -> int * int * int
val sub_list : 'a list -> int -> int -> 'a list
val split_list : 'a list -> int -> 'a list * 'a list
val sum_of_list : int list -> int
val string_of_list : Format.formatter -> (Format.formatter -> 'a -> unit) -> 'a list -> unit
val format_mid_string : string -> string -> string -> string


type t

module S : Set.S with type elt = t
type set = S.t

module M : Map.S with type key = t
type 'a map = 'a M.t

val fresh : unit -> t
val to_string : t -> string
val print : Format.formatter -> t -> unit

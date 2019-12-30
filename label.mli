
type t

val fresh : unit -> t

module M : Map.S with type key = t
type 'a map = 'a M.t

module S : Set.S with type elt = t
type set = S.t

val string_of_label : Format.formatter -> t -> unit


type t

val fresh : unit -> t

module S : Set.S with type elt = t
type set = S.t

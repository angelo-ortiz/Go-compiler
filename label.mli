
type t

val fresh : unit -> t

module M : Map.S with type key = t
type 'a map = 'a M.t

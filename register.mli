
type t

val fresh : unit -> t

module S : Set.S with type elt = t
type set = S.t

module M : Map.S with type key = t
type 'a map = 'a M.t

val rax : t (* division quotient *)
val rdx : t (* division remainder *)
val rdi : t (* new & println *)
val rsp : t (* frame pointer *)
val rbp : t (* base pointer *)
val parameters : t list (* first 6 functions actual parameters *)
val callee_saved : t list
val caller_saved : t list

val string_of_reg : Format.formatter -> t -> unit

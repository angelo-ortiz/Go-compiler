
type t

module S : Set.S with type elt = t
type set = S.t

module M : Map.S with type key = t
type 'a map = 'a M.t

val fresh : unit -> t

val rax : t (* division quotient *)
val rbx : t
val rcx : t
val rdx : t (* division remainder *)
val rsi : t
val rdi : t (* new & println *)
val rbp : t (* base pointer *)
val rsp : t (* frame pointer *)
val r8 : t
val r9 : t
val r10 : t
val r11 : t
val r12 : t
val r13 : t
val r14 : t
val r15 : t

val tmp1 : t (* temporary register *)
val tmp2 : t (* temporary register *)

val parameters : t list (* first six actual parameters of a function *)
val callee_saved : t list
val caller_saved : t list
val allocable : set  (* working registers *)

val string_of_reg : Format.formatter -> t -> unit

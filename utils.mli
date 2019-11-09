
exception Syntax_error of string * (Lexing.position * Lexing.position)

val red : string
val yellow : string
val blue : string
val invert : string
val close : string

val string_of_unop : Format.formatter -> Ast.unop -> unit
val string_of_binop : Format.formatter -> Ast.binop -> unit
val string_of_constant : Format.formatter -> Ast.constant -> unit
val string_of_expr : Format.formatter -> Ast.expr -> unit

val get_ident : Lexing.position * Lexing.position -> Ast.expr -> Ast.ident
val add_int_pos : Lexing.position * Lexing.position -> unit

val check_package :
  Ast.expr ->
  Lexing.position * Lexing.position ->
  string -> Lexing.position * Lexing.position -> unit
val run_check : Ast.block -> Ast.block

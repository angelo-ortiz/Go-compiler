
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

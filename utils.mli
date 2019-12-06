
exception Syntax_error of (Lexing.position * Lexing.position) * string

val red : string
val yellow : string
val blue : string
val invert : string
val close : string

val incr_level : unit -> unit
val decr_level : unit -> unit
val format_mid_string : string -> string -> string -> string
val string_of_unop : Format.formatter -> Ast.unop -> unit
val string_of_binop : Format.formatter -> Ast.binop -> unit
val string_of_constant : Format.formatter -> Ast.constant -> unit
val string_of_expr : Format.formatter -> Ast.desc -> unit
val string_of_texpr : Format.formatter -> Ty_ast.tdesc -> unit
val get_ident : Ast.expr -> Ast.ident
val check_package : Ast.expr -> string -> Ast.loc -> unit
val check_int : string -> Ast.loc -> Big_int.big_int
val check_neg_int : Ast.expr -> Ast.expr

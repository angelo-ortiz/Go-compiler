
exception Syntax_error of Utils.loc * string

val syntax_error : Utils.loc -> string -> 'a
val incr_level : unit -> unit
val decr_level : unit -> unit
val check_package : Ast.expr -> string -> Utils.loc -> unit
val check_int : string -> Utils.loc -> Big_int_Z.big_int
val check_neg_int : Ast.expr -> Ast.expr
val get_ident : Ast.expr -> Ast.ident
val string_of_unop : Format.formatter -> Ast.unop -> unit
val string_of_binop : Format.formatter -> Ast.binop -> unit
val string_of_constant : Format.formatter -> Ast.constant -> unit
val string_of_expr : Format.formatter -> Ast.desc -> unit

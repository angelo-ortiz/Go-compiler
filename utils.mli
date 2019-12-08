
exception Syntax_error of (Lexing.position * Lexing.position) * string

val red : string
val yellow : string
val blue : string
val invert : string
val close : string

val syntax_error : Ast.loc -> string -> 'a
val incr_level : unit -> unit
val decr_level : unit -> unit
val position_of_loc : Ast.loc -> int * int * int
val format_mid_string : string -> string -> string -> string
val string_of_unop : Format.formatter -> Ast.unop -> unit
val string_of_binop : Format.formatter -> Ast.binop -> unit
val string_of_constant : Format.formatter -> Ast.constant -> unit
val string_of_expr : Format.formatter -> Ast.desc -> unit
val string_of_type_list : Format.formatter -> Ty_ast.t_typ list -> unit
val string_of_type : Format.formatter -> Ty_ast.t_typ -> unit
val string_of_texpr : Format.formatter -> Ty_ast.tdesc -> unit
val list_fst_rev : ('a  * 'b) list -> 'a list -> 'a list 
val get_ident : Ast.expr * Ast.loc -> Ast.ident
val check_package : Ast.expr -> string -> Ast.loc -> unit
val check_int : string -> Ast.loc -> Big_int.big_int
val check_neg_int : Ast.expr -> Ast.expr

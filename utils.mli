
exception Syntax_error of Ast.loc * string
exception Type_error of Ast.loc * string
                      
val red : string
val yellow : string
val blue : string
val invert : string
val close : string
val dummy_loc : Ast.loc
val word_size : int
  
val syntax_error : Ast.loc -> string -> 'a
val type_error : Ast.loc -> string -> 'a
val incr_level : unit -> unit
val decr_level : unit -> unit
val position_of_loc : Ast.loc -> int * int * int
val list_fst_rev : ('a  * 'b) list -> 'a list -> 'a list 
val sub_list : 'a list -> int -> int -> 'a list
val flatten : 'a list list -> 'a list list
val sum_of_list : int list -> int
val check_package : Ast.expr -> string -> Ast.loc -> unit
val check_int : string -> Ast.loc -> Big_int.big_int
val check_neg_int : Ast.expr -> Ast.expr
val format_mid_string : string -> string -> string -> string
val string_of_unop : Format.formatter -> Ast.unop -> unit
val string_of_binop : Format.formatter -> Ast.binop -> unit
val string_of_constant : Format.formatter -> Ast.constant -> unit
val string_of_expr : Format.formatter -> Ast.desc -> unit
val string_of_type_list : Format.formatter -> Asg.t_typ list -> unit
val string_of_type : Format.formatter -> Asg.t_typ -> unit
val string_of_texpr : Format.formatter -> Asg.texpr -> unit
val length_of_type : Asg.t_typ -> int
val get_ident : Ast.expr * Ast.loc -> Ast.ident
val binop_expected_type : Ast.binop -> Asg.t_typ option
val verify_operand_type : Ast.binop -> Ast.loc -> Ast.desc -> Asg.t_typ option * Asg.t_typ -> Asg.t_typ
val single_texpr_compatible_types : Asg.t_typ -> Asg.t_typ -> Ast.loc -> (unit -> string) -> unit
val multi_texpr_compatible_types :  Asg.t_typ -> Asg.texpr -> string -> Asg.texpr
val check_fun_return : Asg.tfundef Asg.Smap.t -> unit
val check_recursive_struct : Asg.tstrdef Asg.Smap.t -> unit
val check_fun_main : (string * Ast.loc * Ast.vars list * Ast.typ list * Ast.block) list -> unit
val prefix : int -> 'a list -> 'a list
val inv_ubranch : Rtltree.mubranch -> Rtltree.mubranch
val inv_bbranch : Rtltree.mbbranch -> Rtltree.mbbranch

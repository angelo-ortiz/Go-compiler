
open Format
   
open Ast
open Ty_ast
open Lexing

exception Syntax_error of Ast.loc * string

let red = "\027[31m"
let yellow = "\027[33m"
let blue = "\027[34m"
let invert = "\027[7m"
let close = "\027[0m"
let level = ref 0
let max_int = Big_int.power_int_positive_int 2 63

let syntax_error loc msg =
  raise (Syntax_error (loc, msg))
            
let incr_level () = incr level
let decr_level () = decr level

let position_of_loc (b, e) =
  let line = b.pos_lnum in
  let first_char = b.pos_cnum - b.pos_bol + 1 in
  let last_char = e.pos_cnum - e.pos_bol in
  line, first_char, last_char

let format_mid_string left centre right =
  Format.sprintf "%s%s%s%s%s%s%s" left invert yellow centre close close right

let string_of_unop fmt = function
  | Unot -> Format.fprintf fmt "!"
  | Uneg -> Format.fprintf fmt "-"
  | Udref -> Format.fprintf fmt "*"
  | Uaddr -> Format.fprintf fmt "&"
													  
let string_of_binop fmt = function
  | Badd -> Format.fprintf fmt "+" | Bsub -> Format.fprintf fmt "-"
  | Bmul -> Format.fprintf fmt "*" | Bdiv -> Format.fprintf fmt "/"
  | Bmod -> Format.fprintf fmt "%%" | Beq -> Format.fprintf fmt "=="
  | Bneq -> Format.fprintf fmt "!=" | Blt -> Format.fprintf fmt "<"
  | Ble -> Format.fprintf fmt "<=" | Bgt -> Format.fprintf fmt ">"
  | Bge -> Format.fprintf fmt ">=" | Band -> Format.fprintf fmt "&&"
  | Bor -> Format.fprintf fmt "||"
									  
let string_of_constant fmt = function
  | Cint n -> Format.fprintf fmt "%s" (Big_int.string_of_big_int n)
  | Cstring str -> Format.fprintf fmt "%s" str
  | Cbool b -> Format.fprintf fmt (if b then "true" else "false")
  | Cnil -> Format.fprintf fmt "nil"
			  
let rec string_of_expr fmt = function
  | Ecst cst ->
     Format.fprintf fmt "%a" string_of_constant cst
  | Eident id ->
     Format.fprintf fmt "%s" id
  | Eselect (exp, (field, _)) ->
     Format.fprintf fmt "%a.%s" string_of_expr exp.desc field
  | Ecall (f, args) ->
     Format.fprintf fmt "%s()" f 
  | Eprint _ ->
     Format.fprintf fmt "fmt.Print()"
  | Eunop (op, expr) ->
     Format.fprintf fmt "%a(%a)" string_of_unop op string_of_expr expr.desc
  | Ebinop (op, l, r) ->
     Format.fprintf fmt "@[(%a %a@ %a)@]"
       string_of_expr l.desc string_of_binop op string_of_expr r.desc

let rec string_of_type_list fmt tl =
  Format.fprintf fmt "(";
  List.iteri
    (fun i t -> Format.fprintf fmt "%s%a" (if i > 0 then "," else "") string_of_type t) tl;
  Format.fprintf fmt ")"
  
and string_of_type fmt = function
  | TTint ->
     Format.fprintf fmt "int"
  | TTbool ->
     Format.fprintf fmt "bool"
  | TTstring ->
     Format.fprintf fmt "str"
  | TTnil | TTuntyped ->
     assert false
  | TTunit ->
     Format.fprintf fmt "unit"
  | TTstruct s ->
     Format.fprintf fmt "%s" s
  | TTtuple tl ->
     string_of_type_list fmt tl
  | TTpointer t ->
     Format.fprintf fmt "*%a" string_of_type t

let rec string_of_texpr fmt = function
  | TEint n ->
     Format.fprintf fmt "%s" (Big_int.string_of_big_int n)
  | TEstring str ->
     Format.fprintf fmt "%s" str
  | TEbool b ->
     Format.fprintf fmt "%s" (if b then "true" else "false")
  | TEnil ->
     Format.fprintf fmt "nil"
  | TEnew typ ->
     Format.fprintf fmt "new(%a)" string_of_type typ
  | TEident id ->
     Format.fprintf fmt "%s" id
  | TEselect (struct_, field) ->
     Format.fprintf fmt "%a.%s" string_of_texpr struct_.tdesc field
  | TEcall (f, args) ->
     Format.fprintf fmt "%s()" f 
  | TEprint _ ->
     Format.fprintf fmt "fmt.Print()"
  | TEunop (op, texpr) ->
     Format.fprintf fmt "%a(%a)" string_of_unop op string_of_texpr texpr.tdesc
  | TEbinop (op, l, r) ->
     Format.fprintf fmt "@[(%a %a@ %a)@]"
       string_of_texpr l.tdesc string_of_binop op string_of_texpr r.tdesc

let rec list_fst_rev rem acc =
  match rem with 
  | [] ->
     List.rev acc
  | e :: rem ->
     list_fst_rev rem (fst e :: acc)
    
let get_ident (e, loc)  =
  match e.desc, loc with
  | Eident id, _ ->
     id, loc
  | _ as exp, _ ->
	 syntax_error e.loc
       (Format.asprintf "unexpected expression %s%s%a%s%s, expecting string"
		  invert yellow string_of_expr exp close close)
    
let check_package pkg func func_loc =
  match pkg.desc with
  | Eident id when id = "fmt" ->
	 if func <> "Print" then
       syntax_error func_loc
         (Format.sprintf "unexpected function %s%s%s%s%s, expecting Print"
            invert yellow func close close)
  | _ ->
     syntax_error pkg.loc "expected package fmt"

let overflow n =
  Big_int.ge_big_int n max_int
  
let underflow =
  let min_int = Big_int.minus_big_int max_int in
  fun n -> Big_int.lt_big_int n min_int
         
let check_int_size n loc =
  if !level =  0 && (overflow n || underflow n) then
    syntax_error loc
      (Format.sprintf "%s%s%s%s%s does not fit in 64 bits"
		 invert yellow (Big_int.string_of_big_int n) close close)
  
let check_int n_str loc =
  let n = Big_int.big_int_of_string n_str in
  check_int_size n loc;
  if !level land 1 = 1 then Big_int.minus_big_int n
  else n
  
let check_neg_int e =
  match e.desc with
  | Eunop (Uneg, arg) ->
     begin
	   match arg.desc with
	   | Ecst (Cint n) ->
          check_int_size n e.loc;
          { arg with loc = e.loc }
       | _ ->
          e
	 end
  | _ ->
     assert false
    


open Format
   
open Ast
open Lexing

exception Syntax_error of string * Ast.loc

let red = "\027[31m"
let yellow = "\027[33m"
let blue = "\027[34m"
let invert = "\027[7m"
let close = "\027[0m"
let level = ref 0
let max_int = Big_int.power_int_positive_int 2 63

let incr_level () = incr level
let decr_level () = decr level

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
  | Eaccess (exp, field) ->
     Format.fprintf fmt "%a.%s" string_of_expr exp.expr field
  | Ecall (f, args) ->
     Format.fprintf fmt "%s(...)" f 
  | Eprint _ ->
     Format.fprintf fmt "fmt.Print(...)"
  | Eunop (op, expr) ->
     Format.fprintf fmt "%a(%a)" string_of_unop op string_of_expr expr.expr
  | Ebinop (op, l, r) ->
     Format.fprintf fmt "@[(%a %a@ %a)@]"
       string_of_expr l.expr string_of_binop op string_of_expr r.expr

let get_ident e =
  match e.expr with
  | Eident id ->
     id
  | _ as exp ->
	 raise ( Syntax_error
			   (Format.asprintf "unexpected expression %s%s%a%s%s, expecting string"
				  invert yellow string_of_expr exp close close, e.loc) )
    
let check_package pkg func func_loc =
  match pkg.expr with
  | Eident id when id = "fmt" ->
	 if func <> "Print" then
       raise ( Syntax_error
                 (Format.sprintf "unexpected function %s%s%s%s%s, expecting Print"
				    invert yellow func close close, func_loc) )
  | _ ->
     raise (Syntax_error ("expected package fmt", pkg.loc))

let overflow n =
  Big_int.ge_big_int n max_int
  
let underflow =
  let min_int = Big_int.minus_big_int max_int in
  fun n -> Big_int.lt_big_int n min_int
         
let check_int_size n loc =
  if !level =  0 && (overflow n || underflow n) then
    raise ( Syntax_error
			  (Format.sprintf "%s%s%s%s%s does not fit in 64 bits"
				 invert yellow (Big_int.string_of_big_int n) close close, loc) )
  
let check_int n_str loc =
  let n = Big_int.big_int_of_string n_str in
  check_int_size n loc;
  if !level land 1 = 1 then Big_int.minus_big_int n
  else n
  
let check_neg_int e =
  match e.expr with
  | Eunop (Uneg, arg) ->
     begin
	   match arg.expr with
	   | Ecst (Cint n) ->
          check_int_size n e.loc;
          { arg with loc = e.loc }
       | _ ->
          e
	 end
  | _ ->
     assert false
    

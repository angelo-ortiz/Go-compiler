
open Format
   
open Ast
open Lexing

exception Syntax_error of string * (Lexing.position * Lexing.position)

let red = "\027[31m"
let yellow = "\027[33m"
let blue = "\027[34m"
let invert = "\027[7m"
let close = "\027[0m"

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
     Format.fprintf fmt "%a.%s" string_of_expr exp field
  | Ecall (f, args) ->
     Format.fprintf fmt "%s(...)" f 
  | Eprint _ ->
     Format.fprintf fmt "fmt.Print(...)"
  | Eunop (op, expr) ->
     Format.fprintf fmt "%a%a" string_of_unop op string_of_expr expr
  | Ebinop (op, l, r) ->
     Format.fprintf fmt "@[%a %a@ %a@]"
       string_of_expr l string_of_binop op string_of_expr r
						                                               
	                                                                   
                                                                       


open Format
   
open Ast
open Lexing

exception Syntax_error of string * (Lexing.position * Lexing.position)

let red = "\027[31m"
let yellow = "\027[33m"
let blue = "\027[34m"
let invert = "\027[7m"
let close = "\027[0m"
let level = ref 0
let max_int = Big_int.power_int_positive_int 2 63
let int_pos = ref []

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

let get_ident pos = function
  | Eident id -> id
  | _ as exp ->
	 raise ( Syntax_error
			   (Format.asprintf "unexpected expression %s%s%a%s%s, expecting string"
				 invert yellow string_of_expr exp close close, pos) )

let overflow n =
  Big_int.ge_big_int n max_int
  
let underflow =
  let min_int = Big_int.minus_big_int max_int in
  fun n -> Big_int.lt_big_int n min_int
         
let add_int_pos pos =
  int_pos := pos :: !int_pos
  
let check_package pkg pkg_p func func_p =
  match pkg with
  | Eident id when id = "fmt" ->
	 if func <> "Print" then
       raise ( Syntax_error
                 (Format.sprintf "unexpected function %s%s%s%s%s, expecting Print"
				    invert yellow func close close, func_p) )
  | _ -> raise (Syntax_error ("expected package fmt", pkg_p))

let check_opt check = function
  | None -> None
  | Some e -> Some (check e)
	                                                                   
let rec check_stmt = function
  | Snop -> Snop
  | Sexec instr -> Sexec (check_shstmt instr)
  | Sblock b -> Sblock (check_block b)
  | Sif (cond, body, othw) ->
	 Sif (check_expr cond, List.map (check_stmt) body, check_else othw)
  | Sinit (vars, ty, vals) -> Sinit (vars, ty, List.map (check_expr) vals)
  | Sreturn vals -> Sreturn (List.map (check_expr) vals)
  | Sfor (init, cond, post, body) ->
	 Sfor (check_opt check_shstmt init, check_expr cond, check_opt check_shstmt post, check_block body)
    
and check_block b = List.map (check_stmt) b

and check_shstmt = function
  | Ieval exp ->
     Ieval (check_expr exp)
  | Iincr exp ->
     Iincr (check_expr exp)
  | Idecr exp ->
     Idecr (check_expr exp)
  | Iset (fields, vals) ->
	 Iset (List.map (check_expr) fields, List.map (check_expr) vals)
  | Iassign (vars, vals) ->
	 Iassign (vars, List.map (check_expr) vals)

and check_expr = function
  | Ecst (Cint n) ->
     Ecst (Cint (check_int n))
  | Ecst _ as e -> e
  | Eident _ as e -> e
  | Eaccess (struct_, field) ->
     Eaccess (check_expr struct_, field)
  | Ecall (f, actuals) ->
     Ecall (f, List.map (check_expr) actuals)
  | Eprint vals ->
     Eprint (List.map (check_expr) vals)
  | Eunop (op, exp) ->
	 begin
	   match op with
	   | Uneg ->
		  begin
			incr level;
			let exp = check_expr exp in
			decr level;
			match exp with
			| Ecst (Cint _) -> exp
			| _ -> Eunop (Uneg, exp)
		  end
	   | _ ->
          Eunop (op, check_expr exp)
	 end
  | Ebinop (op, l, r) ->
     Ebinop (op, check_expr l, check_expr r)
                       
and check_int n =
  let n = if !level land 1 = 1 then Big_int.minus_big_int n else n in
  if overflow n || underflow n
  then raise ( Syntax_error
				 (Format.sprintf "%s%s%s%s%s does not fit in 64 bits"
					invert yellow (Big_int.string_of_big_int n) close close,
				  List.hd !int_pos) )
  else begin
      int_pos := List.tl !int_pos;
      n
    end
  
and check_else = function
  | ELblock b ->
     ELblock (check_block b)
  | ELif (cond, body, othw) ->
	 ELif (check_expr cond, List.map (check_stmt) body, check_else othw)
       
let run_check =
  int_pos := List.rev !int_pos;
  check_block

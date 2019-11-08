
 /* Go parser */

%{
	open Format
	   
	open Ast
	open Utils
	   
	let get_ident pos = function
	  | Eident id -> id
	  | _ as exp ->
		 raise (Utils.Syntax_error
				  (Format.asprintf "unexpected expression %s%s%a%s%s, expecting string"
								   Utils.invert Utils.yellow Utils.string_of_expr exp Utils.close Utils.close, pos))

	let check_package fmt pos_fmt print pos_print =
	  match fmt with
	  | Eident id when id = "fmt" ->
		 if print <> "Print"
		 then raise (Utils.Syntax_error
					   (Format.sprintf "unexpected primitive %s%s%s%s%s, expecting Print"
									   Utils.invert Utils.yellow print Utils.close Utils.close, pos_print))
	  | _ -> raise (Utils.Syntax_error ("expected package fmt", pos_fmt))

	let check_opt check = function
	  | None -> None
	  | Some e -> Some (check e)

	let level = ref 0
	let max_int = Big_int.power_int_positive_int 2 63
	let overflow n =
	  Big_int.ge_big_int n max_int

	let underflow =
	  let min_int = Big_int.minus_big_int max_int in
	  fun n -> Big_int.lt_big_int n min_int 

	let rec check_stmt = function
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
	  | Ieval exp -> Ieval (check_expr exp)
	  | Iincr exp -> Iincr (check_expr exp)
	  | Idecr exp -> Idecr (check_expr exp)
	  | Iset (fields, vals) ->
		 Iset (List.map (check_expr) fields, List.map (check_expr) vals)
	  | Iassign (vars, vals) ->
		 Iassign (vars, List.map (check_expr) vals)
	and check_expr = function
	  | Ecst (Cint n) -> Ecst (Cint (check_int n))
	  | Ecst _ as e -> e
	  | Eident _ as e -> e
	  | Eaccess (struct_, field) -> Eaccess (check_expr struct_, field)
	  | Ecall (f, actuals) -> Ecall (f, List.map (check_expr) actuals)
	  | Eprint vals -> Eprint (List.map (check_expr) vals)
	  | Eunop (op, exp) ->
		 begin
		   match op with
		   | Uneg ->
			  begin
				incr level;
				let exp = check_expr exp in
				decr level;
				match exp with
				| Ecst (Cint n) -> Ecst (Cint (check_int n))
			  | _ -> Eunop (Uneg, exp)
			  end
		   | _ -> Eunop (op, check_expr exp)
		 end
	  | Ebinop (op, l, r) -> Ebinop (op, check_expr l, check_expr r)
	and check_int n =
	  if !level = 0 && (overflow n || underflow n)
	  then raise (Utils.Syntax_error
					(Format.sprintf "%s%s%s%s%s does not fit in 64 bits"
									Utils.invert Utils.yellow (Big_int.string_of_big_int n) Utils.close Utils.close,
					 (Lexing.dummy_pos, Lexing.dummy_pos)));
	  if !level mod 2 = 1 then Big_int.minus_big_int n
	  else n 
	and check_else = function
	  | ELblock b -> ELblock (check_block b)
	  | ELif (cond, body, othw) ->
		 ELif (check_expr cond, List.map (check_stmt) body, check_else othw)

	let struct_name = None
	let field_name = None
	let positions = ref []
					  
%}

%token <Ast.binop> CMP
%token <Ast.constant> CST
%token <string> IDENT
%token ADDR DOT NOT AND OR
%token PLUS MINUS STAR DIV MOD
%token INCR DECR SET ASSIGN
%token IMPORT PACKAGE STRUCT
%token FUNC VAR TYPE RETURN
%token FOR IF ELSE
%token SMCOLON SMCEND COMMA COMMEND
%token BEGIN END LPAR RPAR EOF

%left OR
%left AND
%left CMP
%left PLUS MINUS
%left STAR DIV MOD
%nonassoc unary_minus unary_star ADDR NOT
%left DOT

%start file

%type <Ast.file> file

%%

file:
  PACKAGE SMCOLON imp = boption(IMPORT) decls = decl* EOF
	{ { imp = imp; decls = decls } }
;

decl:
  | TYPE s = IDENT STRUCT BEGIN fields = loption(terminated(separated_nonempty_list(SMCOLON, vars), SMCEND?)) END SMCOLON
	{ Dstruct (s, fields) }
  | FUNC f = IDENT LPAR params = loption(terminated(separated_nonempty_list(COMMA, vars), COMMEND?)) RPAR retty = retty? block = block SMCOLON
	{ Dfunc (f, params, retty, check_block block) }
;

vars:
  vars = separated_nonempty_list(COMMA, IDENT)  ty = ty
	{ vars, ty }
;

ty:
  | ty = IDENT
	{ Tbasic ty }
  | STAR ty = ty
	{ Tpointer ty }
;

retty:
  | ty = ty
	{ RTsingle ty }
  | LPAR tys = separated_nonempty_list(COMMA, ty) COMMEND? RPAR
	{ RTtuple tys }
;

block:
  | BEGIN stmts = separated_nonempty_list(SMCOLON, stmt?) SMCEND? END
	{ List.fold_right (fun x l -> match x with | None -> l | Some y -> y::l) stmts [] }
;

stmt:
  | i = shstmt
	{ Sexec i }
  | b = block
	{ Sblock b }
  | i = stif
	{ Sif i }
  | VAR vars = separated_nonempty_list(COMMA, IDENT) ty = ty?
	{ Sinit (vars, ty, []) }
  | VAR vars = separated_nonempty_list(COMMA, IDENT) ty = ty? SET values = expr_list
	{ Sinit (vars, ty, values) }
  | RETURN vs = separated_list(COMMA, expr)
	{ Sreturn vs }
  | FOR body = block
	{ Sfor (None, Ecst (Cbool true), None, body) }
  | FOR cond = expr body = block
	{ Sfor (None, cond, None, body) }
  | FOR init = shstmt? SMCOLON cond = expr SMCOLON post = shstmt? body= block
	{ Sfor (init, cond, post, body) }

stif:
  | IF e = expr b = block
	{ e, b, ELblock [] }
  | IF e = expr b = block ELSE el = block
	{ e, b, ELblock el }
  | IF e = expr b = block ELSE el = stif
	{ e, b, ELif el }

rev_expr_list:
  | e = expr { positions := [$loc(e)]; [e] }
  | exps = expr_list COMMA e = expr
	{ positions := $loc(e) :: !positions; e :: exps }

expr_list:
  exps = rev_expr_list { List.rev exps }

assign:
  vars = expr_list ASSIGN
	{ List.map2 (get_ident) !positions (List.rev vars) }

shstmt:
  | e = expr
	{ Ieval e }
  | e = expr INCR
	{ Iincr e }
  | e = expr DECR
	{ Idecr e }
  | exps = expr_list SET values = expr_list
	{ Iset (exps, values)  }
  | vars = assign values = expr_list
	{ Iassign (vars, values) }
;

print:
  fmt = expr DOT print = IDENT LPAR
	{ check_package fmt $loc(fmt) print $loc(print) }

expr:
  | c = CST
	{ Ecst c }
  | LPAR e = expr RPAR
	{ e }
  | v = IDENT
	{ Eident v }
  | s = expr DOT f = IDENT
	{ Eaccess (s, f) }
  | f = IDENT actuals = delimited(LPAR, separated_list(COMMA, expr), RPAR)
	{ Ecall (f, actuals) }
  | print values = separated_list(COMMA, expr) RPAR
	{ Eprint values }
  | NOT e = expr
	{ Eunop (Unot, e) }
  | MINUS e = expr %prec unary_minus
	{ Eunop (Uneg, e) }
  | STAR e = expr %prec unary_star
	{ Eunop (Udref, e) }
  | ADDR e = expr
	{ Eunop (Uaddr, e) }
  | l = expr op = binop r = expr
	{ Ebinop (op, l, r) }

%inline binop:
  | PLUS    { Badd }
  | MINUS   { Badd }
  | STAR    { Bmul }
  | DIV     { Bdiv }
  | MOD     { Bmod }
  | AND     { Band }
  | OR      { Bor }
  | c = CMP { c }

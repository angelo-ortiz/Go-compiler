
 /* Go parser */

%{
	open Ast
	   
	let string_of_unop = function
	  | Unot -> "!" | Uneg -> "-" | Udref -> "*" | Uaddr -> "&"
														  
	let string_of_binop = function
	  | Badd -> "+" | Bsub -> "-"  | Bmul -> "*" | Bdiv -> "/" | Bmod -> "%"
	  | Beq -> "==" | Bneq -> "!=" | Blt -> "<" | Ble -> "<=" | Bgt -> ">"
	  | Bge -> ">=" | Band -> "&&" | Bor -> "||"
										  
	let string_of_constant = function
	  | Cint n -> Int64.to_string n
	  | Cstring str -> str
	  | Cbool b -> if b then "true" else "false"
	  | Cnil -> "nil"
			  
	let rec string_of_expr = function
	  | Ecst cst -> string_of_constant cst
	  | Eident id -> "variable " ^ id
	  | Eaccess (exp, field) -> (string_of_expr exp) ^ ".(" ^ field ^ ")"
	  | Ecall (f, args) -> f ^ "(...)"
	  | Eprint _ -> "fmt.Print(...)"
	  | Eunop (op, expr) -> (string_of_unop op) ^ (string_of_expr expr)
	  | Ebinop (op, l, r) -> (string_of_expr l) ^ (string_of_binop op) ^ (string_of_expr r)
						   
	let get_ident = function
	  | Eident id -> id
	  | _ as exp -> failwith ("expected an identifier, but got " ^ (string_of_expr exp))
				  
%}

/* %token TYPE RETTYPE */
%token <Ast.binop> CMP
%token <Ast.constant> CST
%token <string> IDENT
%token ADDR DOT NOT AND OR
%token PLUS MINUS STAR DIV MOD
%token INCR DECR SET ASSIGN
%token IMPORT PACKAGE STRUCT
%token FUNC VAR TYPE RETURN
%token FOR IF ELSE PRINT
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
  | TYPE s = IDENT STRUCT BEGIN END SMCOLON
	{ Dstruct (s, []) }
  | TYPE s = IDENT STRUCT BEGIN fields = separated_nonempty_list(SMCOLON, vars) SMCEND? END SMCOLON
	{ Dstruct (s, fields) }
  | FUNC f = IDENT LPAR RPAR retty = retty? block = block SMCOLON
	{ Dfun (f, [], retty, block) }
  | FUNC f = IDENT LPAR params = separated_nonempty_list(COMMA, vars) COMMEND? RPAR retty = retty? block = block SMCOLON
	{ Dfun (f, params, retty, block) }
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
  | BEGIN END
	{ [] }
  | BEGIN stmts = separated_nonempty_list(SMCOLON, stmt) SMCEND? END
	{ stmts }
;

stmt:
  | i = instr
	{ Sexec i }
  | b = block
	{ Sblock b }
  | i = stif
	{ Sif i }
  | VAR vars = separated_nonempty_list(COMMA, IDENT) ty = ty?
	{ Sinit (vars, ty, []) }
  | VAR vars = separated_nonempty_list(COMMA, IDENT) ty = ty? SET values = separated_nonempty_list(COMMA, expr)
	{ Sinit (vars, ty, values) }
  | RETURN vs = separated_list(COMMA, expr)
	{ Sreturn vs }
  | FOR body = block
	{ Sfor (None, Ecst (Cbool true), None, body) }
  | FOR cond = expr body = block
	{ Sfor (None, cond, None, body) }
  | FOR init = instr? SMCOLON cond = expr SMCOLON post = instr? body= block
	{ Sfor (init, cond, post, body) }

stif:
  | IF e = expr b = block
	{ e, b, ELblock [] }
  | IF e = expr b = block ELSE el = block
	{ e, b, ELblock el }
  | IF e = expr b = block ELSE el = stif
	{ e, b, ELif el }

instr:
  | e = expr
	{ Ieval e }
  | e = expr INCR
	{ Iincr e }
  | e = expr DECR
	{ Idecr e }
  | es = separated_nonempty_list(COMMA, expr) SET vs = separated_nonempty_list(COMMA, expr)
	{ Iset (es, vs)  }
  | vars = separated_nonempty_list(COMMA, expr) ASSIGN values = separated_nonempty_list(COMMA, expr)
	{ Iassign (List.map (get_ident) vars, values) }
;

expr:
  | c = CST
	{ Ecst c }
  | LPAR e = expr RPAR
	{ e }
  | v = IDENT
	{ Eident v }
  | s = expr DOT f = IDENT
	{ Eaccess (s, f) }
  | f = IDENT LPAR actuals = separated_list(COMMA, expr) RPAR
	{ Ecall (f, actuals) }
  | PRINT LPAR es = separated_list(COMMA, expr) RPAR
	{ Eprint es }
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

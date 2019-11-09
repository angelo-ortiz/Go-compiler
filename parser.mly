
 /* Go parser */

%{
	open Format
	
	open Ast
	open Utils
	   
	let expr_pos = ref []
%}

%token <Ast.binop> CMP
%token <Ast.constant> CST
%token <string> IDENT
%token <string> INT
%token ADDR DOT NOT AND OR
%token PLUS MINUS STAR DIV MOD
%token INCR DECR SET ASSIGN
%token IMPORT PACKAGE STRUCT
%token FUNC VAR TYPE RETURN
%token FOR IF ELSE
%token SMCOLON COMMA
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

tlist(sep, tok):
  | t = tok
	{ [t] }
  | ts = tlist(sep, tok) sep t = tok
	{ t :: ts }
;

decl:
  | TYPE s = IDENT STRUCT BEGIN
    fields = loption(terminated(tlist(SMCOLON, vars), SMCOLON?))
    END SMCOLON
	{ Dstruct (s, List.rev fields) }
  | FUNC f = IDENT LPAR
    params = loption(terminated(tlist(COMMA, vars), COMMA?))
    RPAR retty = retty? block = block SMCOLON
	{ Dfunc (f, List.rev params, retty, Utils.run_check block) }
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
  | LPAR tys = tlist(COMMA, ty) COMMA? RPAR
	{ RTtuple (List.rev tys) }
;

block:
  | BEGIN stmts = separated_nonempty_list(SMCOLON, stmt) END
	{ List.filter (fun st -> st <> Snop) stmts }
;

stmt:
  | { Snop }
  | i = shstmt
	{ Sexec i }
  | b = block
	{ Sblock b }
  | i = stif
	{ Sif i }
  | VAR vars = separated_nonempty_list(COMMA, IDENT) ty = ty?
	{ Sinit (vars, ty, []) }
  | VAR vars = separated_nonempty_list(COMMA, IDENT) ty = ty?
    SET values = expr_list
	{ Sinit (vars, ty, values) }
  | RETURN vs = separated_list(COMMA, expr)
	{ Sreturn vs }
  | FOR body = block
	{ Sfor (None, Ecst (Cbool true), None, body) }
  | FOR cond = expr body = block
	{ Sfor (None, cond, None, body) }
  | FOR init = shstmt? SMCOLON cond = expr SMCOLON post = shstmt? body= block
	{ Sfor (init, cond, post, body) }
;

stif:
  | IF e = expr b = block
	{ e, b, ELblock [] }
  | IF e = expr b = block ELSE el = block
	{ e, b, ELblock el }
  | IF e = expr b = block ELSE el = stif
	{ e, b, ELif el }
;

rev_expr_list:
  | e = expr { expr_pos := [$loc(e)]; [e] }
  | exps = rev_expr_list COMMA e = expr
	{ expr_pos := $loc(e) :: !expr_pos; e :: exps }
;

expr_list:
  exps = rev_expr_list { List.rev exps }
;

assign:
  vars = rev_expr_list ASSIGN
	{ List.map2 (get_ident) !expr_pos vars }
;

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
	{ Iassign (List.rev vars, values) }
;

print:
  fmt = expr DOT print = IDENT LPAR
	{ Utils.check_package fmt $loc(fmt) print $loc(print) }
;

expr:
  | c = CST
	{ Ecst c }
  | n = INT
	{ Utils.add_int_pos $loc(n); Ecst (Cint (Big_int.big_int_of_string n)) }
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
;

%inline binop:
  | PLUS    { Badd }
  | MINUS   { Badd }
  | STAR    { Bmul }
  | DIV     { Bdiv }
  | MOD     { Bmod }
  | AND     { Band }
  | OR      { Bor }
  | c = CMP { c }

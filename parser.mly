
 /* Go parser  */

%{
  open Ast
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
%token FOR IF ELSE
%token PRINT
%token BEGIN END LPAR RPAR SMCOLON COMMA EOF

%nonassoc IDENT
%nonassoc COMMA
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
  | TYPE s = IDENT STRUCT BEGIN fields = separated_nonempty_list(SMCOLON, vars) END SMCOLON
	{ Dstruct (s, fields) }
  | FUNC f = IDENT LPAR RPAR retty = retty? block = block SMCOLON
	{ Dfun (f, [], retty, block) }
  | FUNC f = IDENT LPAR params = separated_nonempty_list(COMMA, vars) RPAR retty = retty? block = block SMCOLON
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
  | LPAR tys = separated_nonempty_list(COMMA, ty) RPAR
	{ RTtuple tys }
;

block:
  | BEGIN END
	{ [] }
  | BEGIN stmts = separated_nonempty_list(SMCOLON, stmt) END
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
  | vars = separated_nonempty_list(COMMA, IDENT) ASSIGN values = separated_nonempty_list(COMMA, expr)
	{ Iassign (vars, values) }
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

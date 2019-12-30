
 /* Go parser */

%{
	open Ast
	open Utils
	
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
  | PACKAGE SMCOLON import = boption(IMPORT) decls = decl* EOF
	{ { import = import, $loc(import); decls = decls } }
;

rev_tlist(sep, tok):
  | t = tok
	{ [t] }
  | ts = rev_tlist(sep, tok) sep t = tok
	{ t :: ts }
;

rev_poslist(sep, tok):
  | t = tok
	{ [t, $loc(t)] }
  | ts = rev_poslist(sep, tok) sep t = tok
	{ (t, $loc(t)) :: ts }
;

poslist(sep, tok):
  | exps = rev_poslist(sep, tok)
	{ List.rev exps }
;

decl:
  | TYPE s = IDENT STRUCT BEGIN
    fields = loption(terminated(rev_tlist(SMCOLON, vars), SMCOLON?))
    END SMCOLON
	{ Dstruct ((s, $loc(s)), List.rev fields) }
  | FUNC f = IDENT LPAR
    params = loption(terminated(rev_tlist(COMMA, vars), COMMA?))
    RPAR rtype = loption(rtype) block = block SMCOLON
	{ Dfunc ((f, $loc(f)), List.rev params, rtype, block) }
;

vars:
  | vars = poslist(COMMA, IDENT)  typ = typ
	{ vars, typ }
;

typ:
  | typ = IDENT
	{ Tbasic (typ, $loc(typ)) }
  | STAR typ = typ
	{ Tpointer typ }
;

rtype:
  | typ = typ
	{ [typ] }
  | LPAR types = rev_tlist(COMMA, typ) COMMA? RPAR
	{ List.rev types }
;

block:
  | BEGIN stmts = separated_nonempty_list(SMCOLON, stmt) END
	{ List.filter (fun st -> st <> Snop) stmts }
;

stmt:
  | /* empty */
	{ Snop }
  | i = shstmt
	{ Sexec i }
  | b = block
	{ Sblock b }
  | st_if = stif
	{ st_if }
  | VAR vars = poslist(COMMA, IDENT) typ = typ
	{ Sinit (vars, Some typ, []) }
  | VAR vars = poslist(COMMA, IDENT) typ = typ? SET values = expr_list
	{ Sinit (vars, typ, values) }
  | RETURN vs = separated_list(COMMA, expr)
	{ Sreturn vs }
  | FOR body = block
	{ Sfor (None, { desc = Ecst (Cbool true); loc = Lexing.dummy_pos, Lexing.dummy_pos }, None, body) }
  | FOR cond = expr body = block
	{ Sfor (None, cond, None, body) }
  | FOR init = shstmt? SMCOLON cond = expr SMCOLON post = shstmt? body = block
	{ Sfor (init, cond, post, body) }
;

stif:
  | IF e = expr b = block
	{ Sif (e, b, []) }
  | IF e = expr b = block ELSE b_el = block
	{ Sif (e, b, b_el) }
  | IF e = expr b = block ELSE b_el = stif
	{ Sif (e, b, [b_el]) }
;

rev_expr_list:
  | e = expr { [e, $loc(e)] }
  | exps = rev_expr_list COMMA e = expr
	{ (e, $loc(e)) :: exps }
;

expr_list:
  | exps = rev_expr_list { Utils.list_fst_rev exps [] }
;

assign:
  | vars = rev_expr_list ASSIGN
	{ List.map Utils.get_ident vars }
;

shstmt:
  | e = expr
	{ Ieval e }
  /* | print values = separated_list(COMMA, expr) RPAR */
  /* 	{ Iprint values } */
  | e = expr INCR
	{ Iincr e }
  | e = expr DECR
	{ Idecr e }
  | exps = expr_list SET values = expr_list
	{ Iassign (exps, values)  }
  | vars = assign values = expr_list
	{ Ideclare (List.rev vars, values) }
;

print:
  | fmt = expr DOT print = IDENT LPAR
	{ Utils.check_package fmt print $loc(print) }
;

expr:
  | c = CST
	{ { desc = Ecst c; loc = $loc(c) } }
  | n = INT
	{ { desc = Ecst (Cint (Utils.check_int n $loc(n))); loc = $loc(n) } }
  | LPAR e = expr RPAR
	{ e }
  | v = IDENT
	{ { desc = Eident v; loc = $loc(v) } }
  | s = expr DOT f = IDENT
	{ { desc = Eselect (s, (f, $loc(f))); loc = $loc } }
  | f = IDENT actuals = delimited(LPAR, separated_list(COMMA, expr), RPAR)
	{ { desc = Ecall (f, actuals); loc = $loc } }
  | print values = separated_list(COMMA, expr) RPAR
  	{ { desc = Eprint values; loc = $loc } }
  | NOT e = expr
	{ { desc = Eunop (Unot, e); loc = $loc } }
  | midrule(MINUS { Utils.incr_level () }) e = expr %prec unary_minus
	{ Utils.decr_level (); Utils.check_neg_int { desc = Eunop (Uneg, e); loc = $loc } }
  | STAR e = expr %prec unary_star
	{ { desc = Eunop (Udref, e); loc = $loc } }
  | ADDR e = expr
	{ { desc = Eunop (Uaddr, e); loc = $loc } }
  | l = expr op = binop r = expr
	{ { desc = Ebinop (op, l, r); loc = $loc } }
;

%inline binop:
  | PLUS    { Badd }
  | MINUS   { Bsub }
  | STAR    { Bmul }
  | DIV     { Bdiv }
  | MOD     { Bmod }
  | AND     { Band }
  | OR      { Bor }
  | c = CMP { c }

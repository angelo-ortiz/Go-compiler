
 /* Go parser  */

%{
  open Ast
%}

/* %token TYPE RETTYPE */
%token <Ast.binop> CMP
%token <Ast.constant> CST
%token <string> IDENT
%token DREF ADDR DOT NOT AND OR
%token PLUS MINUS TIMES DIV MOD
%token INCR DECR SET ASSIGN
%token IMPORT PACKAGE STRUCT
%token FUNC VAR TYPE RETURN
%token FOR IF ELSE
%token PRINT
%token BEGIN END LPAR RPAR SMCOLON COMMA EOF

%left OR
%left AND
%left CMP
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc unary_minus DREF ADDR NOT
%left DOT

%start file

%type <Ast.file> file

%%

file:
  PACKAGE SMCOLON imp = boption(IMPORT) decls = decl* EOF
	{ { imp = imp; decls = decls } }
;

decl:
  | TYPE s = IDENT STRUCT BEGIN END
	{ Dstruct (s, []) }
  | TYPE s = IDENT STRUCT BEGIN fields = separated_nonempty_list(SMCOLON, vars) SMCOLON? END
	{ Dstruct (s, fields) }
;

vars:
  vars = separated_nonempty_list(COMMA, IDENT)  ty = ty
	{ vars, ty }

ty:
  | ty = IDENT
	{ Tbasic ty }
  | DREF ty = ty
	{ Tpointer ty }

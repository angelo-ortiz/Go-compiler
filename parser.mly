
(* Go parser *)

%{
  open Ast
%}

%token TYPE RETTYPE
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
%token BEGIN END LPAR RPAR SMCOLON

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
  PACKAGE SMCOLON imp = boption(IMPORT) decls = decl EOF
	{ { imp = imp; decls = decls } }
;

decl:
  | TYPE s = IDENT STRUCT BEGIN fields = loption(separated_list(SMCOLON, vars)) END
	{ Dstruct (s, fields) }
  | TYPE s = IDENT STRUCT BEGIN fields = loption(pair(separated_list(SMCOLON, IDENT), SMCOLON)) END
	{ Dstruct (s, fields) }
;



(* Abstract syntax tree *)
(* Parsing grammar *)

type loc = Lexing.position * Lexing.position

type ident = string * loc
         
type typ =
  | Tbasic of ident
  | Tpointer of typ

type unop =
  | Unot   (* not e *)
  | Uneg   (* -e *)
  | Udref  (* *e *)
  | Uaddr  (* &e *)

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* && || *)

type constant =
  | Cint of Big_int_Z.big_int
  | Cstring of string
  | Cbool of bool
  | Cnil

type desc =
  | Ecst of constant
  | Eident of string
  | Eselect of expr * ident
  | Ecall of string * expr list
  | Eprint of expr list
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr

and expr = {
    desc: desc;  (* AST expression *)
    loc: loc;    (* location in code *)
  }
          
and shstmt =
  | Ieval of expr
  | Iincr of expr
  | Idecr of expr
  | Iassign of expr list * expr list   (* = *)
  | Ideclare of ident list * expr list (* := *)
             
and block = stmt list
          
and stmt =
  | Snop
  | Sexec of shstmt
  | Sblock of block
  | Sif of expr * block * block
  | Sinit of ident list * typ option * expr list (* var ... *)
  | Sreturn of expr list
  | Sfor of shstmt option * expr * shstmt option * block

and vars = ident list * typ (* list of identifiers, type *)
         
and decl =
  | Dstruct of ident * vars list                   (* name, list of fields *)
  | Dfunc of ident * vars list * typ list * block  (* name, list of formal arguments, return type, body *)

type file = {
    import : bool * loc;   (* true iff "fmt" was imported *)
    decls : decl list;  (* list of structure/function declarations *)
  }


(* Parsing grammar *)

type ident = string

type loc = Lexing.position * Lexing.position

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
  | Cint of Big_int.big_int
  | Cstring of string
  | Cbool of bool
  | Cnil

type desc =
  | Ecst of constant
  | Eident of ident
  | Eselect of expr * ident * loc
  | Ecall of ident * expr list
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr

and expr = {
    desc: desc;
    loc: loc;
  }
          
and shstmt =
  | Ieval of expr
  | Iprint of expr list
  | Iincr of expr
  | Idecr of expr
  | Iset of expr list * expr list
  | Iassign of ident list * expr list
             
and block = stmt list
          
and stmt =
  | Snop
  | Sexec of shstmt
  | Sblock of block
  | Sif of stif
  | Sinit of ident list * typ option * expr list
  | Sreturn of expr list
  | Sfor of shstmt option * expr * shstmt option * block

and stelse =
  | ELblock of block
  | ELif of stif 
          
and stif = expr * block * stelse

and vars = ident list * typ
         
and decl =
  | Dstruct of ident * vars list
  | Dfunc of ident * vars list * typ list * block

type file = {
    imp : bool;
    decls : decl list;
  }

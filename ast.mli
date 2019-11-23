
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

type expr =
  | Ecst of constant
  | Eident of ident
  | Eaccess of pexpr * ident
  | Ecall of ident * pexpr list
  | Eprint of pexpr list
  | Eunop of unop * pexpr
  | Ebinop of binop * pexpr * pexpr

and pexpr = {
    expr: expr;
    loc: loc;
  }
          
and shstmt =
  | Ieval of pexpr
  | Iincr of pexpr
  | Idecr of pexpr
  | Iset of pexpr list * pexpr list
  | Iassign of ident list * pexpr list
             
and block = stmt list
          
and stmt =
  | Snop
  | Sexec of shstmt
  | Sblock of block
  | Sif of stif
  | Sinit of ident list * typ option * pexpr list
  | Sreturn of pexpr list
  | Sfor of shstmt option * pexpr * shstmt option * block

and stelse =
  | ELblock of block
  | ELif of stif 
          
and stif = pexpr * block * stelse

and vars = ident list * typ
         
and decl =
  | Dstruct of ident * vars list
  | Dfunc of ident * vars list * typ list * block

type file = {
    imp : bool;
    decls : decl list;
  }

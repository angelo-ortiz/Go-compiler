
type ident = string

type ty =
  | Tbasic of ident
  | Tpointer of ty

and retty =
  | RTsingle of ty
  | RTtuple of ty list

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
  | Cint of int64
  | Cstring of string
  | Cbool of bool
  | Cnil

type expr =
  | Ecst of constant
  | Eaccess of expr * ident
  | Eident of ident
  | Ecall of ident * expr list
  | Eprint of expr list
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr

and instr =
  | Ieval of expr
  | Iincr of expr
  | Idecr of expr
  | Iset of expr list * expr list
  | Iassign of ident list * expr list
             
and block = stmt list
          
and stmt =
  | Sexec of instr
  | Sblock of block
  | Sif of stif
  | Sinit of ident list * ty * expr list
  | Sreturn of expr list
  | Sfor of instr * expr * instr * block

and stelse =
  | ELblock of block
  | ELif of stif 
          
and stif = expr * block * stelse

and vars = ident list * ty
         
and decl =
  | Dstruct of ident * vars list
  | Dfun of ident * vars list * retty * block

and file = bool * decl list

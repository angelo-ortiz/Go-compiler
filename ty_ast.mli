
(* Typing grammar *) 

type ident = {
    id : string;
    level : int;
    offset: int
  }

type typ =
  | Tint
  | Tstring
  | Tbool
  | Tstruct of string
  | Tfunc of typ list
  | Tpointer of typ

module Smap : Map.S with type key = string
             
type env = typ Smap.t

type texpr = {
    tdesc : tdesc;
    typ : typ
  }
           
and tdesc =
  | TEint of Big_int.big_int
  | TEstring of string
  | TEbool of bool
  | TEnil
  | TEident of ident
  | TEaccess of string * string
  | TEcall of string * texpr list
  | TEprint of texpr list
  | TEunop of Ast.unop * texpr
  | TEbinop of Ast.binop * texpr * texpr

and tblock = {
    vars : typ Smap.t;
    stmts : tstmt list;
    level : int
  }
          
and tstmt =
  | Snop
  | Sincr of texpr
  | Sdecr of texpr
  | Sblock of tblock
  | Sif of texpr * tblock * tblock
  | Sassign of texpr list * texpr list
  | Sreturn of texpr list
  | Sfor of texpr * tblock

type struct_ = typ Smap.t
type func = typ list * typ * tblock
         
type tfile = {
    structs : struct_ Smap.t;
    functions : func Smap.t
  }

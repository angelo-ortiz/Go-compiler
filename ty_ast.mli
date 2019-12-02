
(* Typing grammar *) 

type typ =
  | Tint
  | Tstring
  | Tbool
  | Tnil
  | Tstruct of string
  | Ttuple of typ list
  | Tpointer of typ

type var = {
    id : string;
    level : int;
    offset: int;
    typ: typ
  }

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
  | TEident of string
  | TEselect of string * string
  | TEcall of string * texpr list
  | TEprint of texpr list
  | TEunop of Ast.unop * texpr
  | TEbinop of Ast.binop * texpr * texpr

and tblock = {
    vars : var Smap.t;
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
type func = typ * typ * tblock
         
type tfile = {
    structs : struct_ Smap.t;
    functions : func Smap.t
  }

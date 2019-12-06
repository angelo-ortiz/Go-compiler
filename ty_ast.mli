
(* Typing grammar *) 

type typ =
  | Tint
  | Tstring
  | Tbool
  | Tnil
  | Tunit
  | Tstruct of string
  | Ttuple of typ list (* >= 2 types *)
  | Tpointer of typ

type var = {
    id : string;
    mutable level : int;
    mutable offset: int;
    typ: typ
  }

module Smap : Map.S with type key = string
             
type env = typ Smap.t

type texpr = {
    tdesc : tdesc;
    typ : typ;
    is_assignable : bool;
    loc : Ast.loc;
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
  | TSnop
  | TScall of string * texpr list
  | TSprint of texpr list
  | TSincr of texpr
  | TSdecr of texpr
  | TSblock of tblock
  | TSif of texpr * tblock * tblock
  | TSassign of texpr list * texpr list
  | TSdeclare of texpr list * texpr list
  | TSreturn of texpr list
  | TSfor of texpr * tblock

type struct_ = typ Smap.t
type func = typ * typ * tblock
         
type tfile = {
    structs : struct_ Smap.t;
    functions : func Smap.t
  }

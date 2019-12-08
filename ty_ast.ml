
type t_typ =
  | TTint
  | TTstring
  | TTbool
  | TTnil
  | TTunit
  | TTuntyped
  | TTstruct of string
  | TTtuple of t_typ list (* >= 2 types *)
  | TTpointer of t_typ

type tvar = {
    id : string;
    level : int;
    mutable offset: int;
    typ: t_typ;
    loc : Ast.loc;
  }

module Smap = Map.Make(String)

type env = t_typ Smap.t

type texpr = {
    tdesc : tdesc;
    typ : t_typ;
    is_assignable : bool;
    loc : Ast.loc;
  }
           
and tdesc =
  | TEint of Big_int.big_int
  | TEstring of string
  | TEbool of bool
  | TEnil
  | TEnew of t_typ
  | TEident of string
  | TEselect of texpr * string
  | TEcall of string * texpr list
  | TEprint of texpr list
  | TEunop of Ast.unop * texpr
  | TEbinop of Ast.binop * texpr * texpr

and tblock = {
    vars : tvar Smap.t;
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
  | TSdeclare of tvar list * texpr list
  | TSreturn of texpr list
  | TSfor of texpr * tblock

type fblock =
  | Untyped of Ast.block
  | Typed of tblock
           
type struct_ = t_typ Smap.t * Ast.loc
type func = (string * t_typ) list * t_typ * fblock * Ast.loc
         
type tfile = {
    structs : struct_ Smap.t;
    functions : func Smap.t
  }

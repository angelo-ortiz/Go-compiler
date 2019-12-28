
(* Abstract semantic graph *)
(* Typing grammar *) 

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
    b_number : int;
    typ: t_typ;
    loc : Ast.loc
  }

module Smap : Map.S with type key = string
type 'a smap = 'a Smap.t
             
type texpr = {
    tdesc : tdesc;
    typ : t_typ;
    is_assignable : bool;
    loc : Ast.loc;
  }
           
and tdesc =
  | TEint of int64
  | TEstring of string
  | TEbool of bool
  | TEnil
  | TEnew of t_typ
  | TEident of tvar
  | TEselect of texpr * string
  | TEcall of string * texpr list
  | TEprint of texpr list
  | TEunop of Ast.unop * texpr
  | TEbinop of Ast.binop * texpr * texpr

and tblock = {
    vars : tvar smap;
    stmts : tstmt list;
    number : int;
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

type decl_struct = {
    fields : (string * t_typ) list;
    loc : Ast.loc;
  }
                 
type decl_fun = {
    formals : (string * t_typ) list;
    rtype : t_typ;
    body : fblock;
    loc : Ast.loc;
  }
         
type tfile = {
    structs : decl_struct smap;
    functions : decl_fun smap;
  }

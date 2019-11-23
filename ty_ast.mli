
(* Typing grammar *) 

type ident = {
    id : string;
    level : int;
    offset: int
  }

type term =
  | Tint
  | Tbool
  | Tstring
  | Tstruct
  | Tpointer of term
  | Tvar of tvar
          
and tvar = {
    id : int;
    mutable def: term option
  }

module QVar : sig
  type t = tvar
  val compare : t -> t -> bool
  val equal : t -> t -> bool
  val create : unit -> t
end

module QVset : Set.S with type elt = QVar.t
module Smap : Map.S with type key = string
module QVmap : Map.S with type key = QVar.t
             
type scheme = {
    qvars : QVset.t;
    term : term
  }

type env = {
    bindings : scheme Smap.t;
    fvars : QVset.t
  }

type texpr = {
    expr : expr;
    typ : term
  }
           
and expr =
  | Ecst of Ast.constant
  | Eident of ident
  | Eaccess of string * string
  | Ecall of string * texpr list
  | Eprint of texpr list
  | Eunop of Ast.unop * texpr
  | Ebinop of Ast.binop * texpr * texpr

and block = {
    vars : term Smap.t;
    stmts : stmt list;
    level : int
  }
          
and stmt =
  | Snop
  | Sincr of texpr
  | Sdecr of texpr
  | Sblock of block
  | Sif of texpr * block * block
  | Sassign of texpr list * texpr list
  | Sreturn of texpr list
  | Sfor of texpr * block

type struct_ = term Smap.t
type func = term Smap.t * term list * block
         
type file = {
    structs : struct_ Smap.t;
    functions : func Smap.t
  }

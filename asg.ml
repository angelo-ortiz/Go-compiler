
type t_typ =
  | TTint
  | TTstring
  | TTbool
  | TTnil                  (* type of nil before "unification" *)
  | TTunit                 (* return type = [] *)
  | TTuntyped              (* for Go's type inference in declarations *)
  | TTstruct of string
  | TTtuple of t_typ list  (* return type = _ :: _ :: [] *)
  | TTpointer of t_typ

type tvar = {
    id : string;          (* variable name *)
    b_number : int;       (* block's number *)
    typ: t_typ;           (* type *)
    loc : Ast.loc;        (* location in code *)
  }

module Smap = Map.Make(String)
type 'a smap = 'a Smap.t

type texpr = {
    tdesc : tdesc;         (* typed expression *)
    typ : t_typ;           (* type *)
    is_assignable : bool;  (* true iff left-value *)
    loc : Ast.loc;         (* location in code *)
  }
           
and tdesc =
  | TEint of int64
  | TEstring of string
  | TEbool of bool
  | TEnil
  | TEnew of t_typ  (* the function new is simulated on the go *)
  | TEident of tvar
  | TEselect of texpr * string
  | TEcall of string * texpr list
  | TEprint of texpr list
  | TEunop of Ast.unop * texpr
  | TEbinop of Ast.binop * texpr * texpr

and tblock = {
    vars : tvar smap;    (* declared variables *)
    stmts : tstmt list;  (* list of statements *)
    number : int;        (* block's number *)
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
    fields : (string * t_typ) list;  (* list of (field, type) *)
    loc : Ast.loc;                   (* declaration locus *)
  }
                 
type decl_fun = {
    formals: (string * t_typ) list;  (* list of (formal, type)*)
    rtype : t_typ;                   (* return type *)
    body : fblock;                   (* body *)
    loc : Ast.loc;                   (* declaration locus *)
  }
         
type tfile = {
    structs : decl_struct smap;
    functions : decl_fun smap;
  }

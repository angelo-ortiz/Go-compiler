
(* Instruction selection grammar *) 

type ident = string

type munop =
  | Mnot | Mneg | Maddi of int64 | Mimuli of int64 | Minc | Mdec
  | Midivil of int64 | Midivir of int64 | Mmodil of int64 | Mmodir of int64
  | Msetei of int64 | Msetnei of int64 | Msetgi of int64 | Msetgei of int64
  | Msetli of int64 | Msetlei of int64

type mbinop =
  | Madd | Msub | Mimul | Midiv | Mmod | Mxor
  | Msete | Msetne | Msetg | Msetge | Msetl | Msetle
  | Mmov
           
type iexpr = {
    length : int;    (* number of 8-byte blocks *)
    desc : idesc;    (* selected instruction *)
    typ: Asg.t_typ;  (* type *)
  }
           
and idesc =
  | IEint of int64
  | IEstring of string
  | IEbool of bool
  | IEnil
  | IElist of iexpr list
  | IEmalloc of int32
  | IEaccess of ident
  | IEselect of iexpr * int  (* C's "." *)
  | IEload of iexpr * int    (* C's "->" *)
  | IEcall of ident * iexpr list
  | IEaddr of iexpr
  | IEunop of munop * iexpr
  | IEbinop of mbinop * iexpr * iexpr
  | IEand of iexpr * iexpr
  | IEor of iexpr * iexpr

type assign = {
    length : int; (* number of 8-byte blocks *)
    assignee : assignee;
  }

and assignee =
  | Avar of ident
  | Afield_var of ident * int (* e.n <- && e is an lvar *)
  | Afield of iexpr * int     (* e.n <- *)
  | Adref of iexpr * int      (* e->n <- | *e <- *)

type istmt =
  | ISexpr of iexpr
  | IScall of ident * iexpr list
  | ISprint of iexpr list
  | ISif of iexpr * istmt list * istmt list
  | ISassign of assign list * iexpr list
  | ISreturn of iexpr list
  | ISfor of iexpr * istmt list

type istrdef = (string * Asg.t_typ) list
                 
type ifundef = {
    formals : (ident * int) list;
    result : int list; (* list of results' size in 8-byte blocks *)
    locals : (ident * int) list;
    body: istmt list;
  }
         
type iprogramme = {
    structs : istrdef Utils.smap;
    functions: ifundef Utils.smap;
  }

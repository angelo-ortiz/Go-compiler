
(* Instruction selection grammar *) 

type ident = string

type munop =
  | Mnot | Mneg | Maddi of int32 | Mimuli of int32 | Minc | Mdec
  | Midivil of int32 | Midivir of int32 | Mmodil of int32 | Mmodir of int32
  | Msetei of int32 | Msetnei of int32 | Msetgi of int32 | Msetgei of int32
  | Msetli of int32 | Msetlei of int32

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
  | IEint of int32
  | IEstring of string
  | IEbool of bool
  | IEnil
  | IElist of iexpr list
  | IEmalloc of int32
  | IEaccess of ident
  | IEselect of iexpr * int (* C's "." *)
  | IEload of iexpr * int (* C's "->" *)
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
  | Afield of iexpr * int (* e.n <- *)
  | Adref of iexpr * int (* e->n <- | *e <- *)

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
    structs : istrdef Asg.smap;
    functions: ifundef Asg.smap;
  }

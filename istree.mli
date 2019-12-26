
(* Instruction selection grammar *) 

type ident = string

type munop =
  | Mnot | Mneg | Mdref | Maddr
  | Maddi of int64 | Mimuli of int64 | Minc | Mdec
  | Midivil of int64 | Midivir of int64 | Mmodil of int64 | Mmodir of int64
  | Msal of int64 | Msar of int64 | Mshr of int64 | Mcall of ident
  | Msetei of int64 | Msetnei of int64 | Msetgi of int64 | Msetgei of int64
  | Msetli of int64 | Msetlei of int64

type mbinop =
  | Madd | Msub | Mimul | Midiv | Mmod
  | Msete | Msetne | Msetg | Msetge | Msetl | Msetle
  | Mmov
           
type iexpr =
  | IEint of int64
  | IEstring of string
  | IEbool of bool
  | IEnil
  | IEmalloc of int
  | IEaccess of ident
  | IEload of iexpr * int
  | IEcall of ident * iexpr list
  | IEprint of iexpr list
  | IEunop of munop * iexpr
  | IEbinop of mbinop * iexpr * iexpr
  | IEand of iexpr * iexpr
  | IEor of iexpr * iexpr

and istmt =
  | ISexpr of iexpr
  | IScall of ident * iexpr list
  | ISprint of iexpr list
  | ISblock of istmt list
  | ISif of iexpr * istmt list * istmt list
  | ISassign of iexpr list * iexpr list
  | ISreturn of iexpr list
  | ISfor of iexpr * istmt list

type idecl_struct = int
                 
type idecl_fun = {
    formals : ident list;
    result : int;
    locals : ident list;
    body: istmt list;
  }
         
type ifile = {
    structs : idecl_struct Asg.smap;
    functions : idecl_fun Asg.smap;
  }

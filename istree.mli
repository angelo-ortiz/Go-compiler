
(* Instruction selection grammar *) 

type ident = string

type munop =
  | Mnot | Mneg | Mdref | Maddr | Mxor
  | Maddi of int32 | Mimuli of int32 | Minc | Mdec
  | Midivil of int32 | Midivir of int32 | Mmodil of int32 | Mmodir of int32
  | Msetei of int32 | Msetnei of int32 | Msetgi of int32 | Msetgei of int32
  | Msetli of int32 | Msetlei of int32

type mbinop =
  | Madd | Msub | Mimul | Midiv | Mmod
  | Msete | Msetne | Msetg | Msetge | Msetl | Msetle
  | Mmov
           
type iexpr =
  | IEint of int32
  | IEstring of string
  | IEbool of bool
  | IEnil
  | IEmalloc of int
  | IEaccess of ident
  | IEload of iexpr * int
  | IEcall of ident * iexpr list
  | IEunop of munop * iexpr
  | IEbinop of mbinop * iexpr * iexpr
  | IEand of iexpr * iexpr
  | IEor of iexpr * iexpr

type assign =
  | Avar of ident
  | Afield of iexpr * int
  | Adref of iexpr

type istmt =
  | ISexpr of iexpr
  | IScall of ident * iexpr list
  | ISprint of string * iexpr list
  | ISif of iexpr * istmt list * istmt list
  | ISassign of assign list * iexpr list
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

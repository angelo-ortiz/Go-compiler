
(* Explicit register transfer language grammar *)

type emunop =
  | Mnot | Mneg | Maddi of int64 | Mimuli of int64 | Minc | Mdec
  | Msetei of int64 | Msetnei of int64 | Msetgi of int64 | Msetgei of int64
  | Msetli of int64 | Msetlei of int64

type embinop =
  | Madd | Msub | Mimul | Mxor
  | Msete | Msetne | Msetg | Msetge | Msetl | Msetle
  | Mmov
           
type einstr =
  | Iint of int64 * Register.t * Label.t
  | Istring of string * Register.t * Label.t
  | Ibool of bool * Register.t * Label.t
  | Ilea_local of Register.t * int * Register.t * Label.t (* local Register.t | offset | dst *)
  | Ilea of Register.t * int * Register.t * Label.t (* src | offset | dst *)
  | Iload of Register.t * int * Register.t * Label.t (* src | offset | dst *)
  | Istore of Register.t * Register.t * int * Label.t (* src | dst | offset *)
  | Icall of string * int * Label.t (* # args in regs *)
  | Imunop of emunop * Register.t * Label.t
  | Iidiv_imm of int64 * Label.t
  | Iidiv of Register.t * Label.t
  | Iinc_dec of Rtl.inc_dec * Register.t * int * Label.t (* src | offset *)
  | Imbinop of embinop * Register.t * Register.t * Label.t
  | Imubranch of Rtl.mubranch * Register.t * Label.t * Label.t (* true | false *)
  | Imbbranch of Rtl.mbbranch * Register.t * Register.t * Label.t * Label.t (* 2nd arg | 1st arg *)
  | Igoto of Label.t
  | Ialloc_frame of Label.t
  | Ifree_frame of Label.t
  | Iget_param of int * Register.t * Label.t
  | Iset_result of Register.t * int * Label.t
  | Ipush_param of Register.t * Label.t
  | Ipop_param of Register.t * Label.t
  | Ialloc_stack of int32 * Label.t
  | Ifree_stack of int32 * Label.t
  | Ireturn

type efundef = {
    formals : int;
    locals : Register.set;
    stored_locals : Register.t list list;
    entry : Label.t;
    body : einstr Label.map;
  }

type eprogramme = efundef Utils.smap


(* Explicit register transfer language grammar *)

type register = Register.t
type label = Label.t

type munop =
  | Mnot | Mneg | Maddi of int32 | Mimuli of int32 | Minc | Mdec
  | Msetei of int32 | Msetnei of int32 | Msetgi of int32 | Msetgei of int32
  | Msetli of int32 | Msetlei of int32

type mbinop =
  | Madd | Msub | Mimul | Mxor
  | Msete | Msetne | Msetg | Msetge | Msetl | Msetle
  | Mmov
           
type instr =
  | Iint of int32 * register * label
  | Istring of string * register * label
  | Ibool of bool * register * label
  | Ilea_local of register * int * register * label (* local register | offset | dst *)
  | Ilea of register * int * register * label (* src | offset | dst *)
  | Iload of register * int * register * label (* src | offset | dst *)
  | Istore of register * register * int * label (* src | dst | offset *)
  | Icall of string * int * label (* # args in regs *)
  | Imunop of munop * register * label
  | Iidiv_imm of int32 * label
  | Iidiv of register * label
  (* | Iinc_dec_local of Rtltree.inc_dec * register list * int * label (\* local register | offset *\) *)
  | Iinc_dec of Rtltree.inc_dec * register * int * label (* src | offset *)
  | Imbinop of mbinop * register * register * label
  | Imubranch of Rtltree.mubranch * register * label * label (* true | false *)
  | Imbbranch of Rtltree.mbbranch * register * register * label * label (* 2nd arg | 1st arg *)
  | Igoto of label
  | Ialloc_frame of label
  | Ifree_frame of label
  | Iget_param of int * register * label
  | Iset_result of register * int * label
  | Ipush_param of register * label
  | Ipop_param of register * label
  | Ialloc_stack of int32 * label
  | Ifree_stack of int32 * label
  | Ireturn

type decl_fun = {
    formals : int;
    locals : Register.set;
    stored_locals : register list list;
    entry : label;
    body : instr Label.map;
  }

type file = decl_fun Asg.smap

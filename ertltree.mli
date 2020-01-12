
(* Explicit register transfer language grammar *)

type register = Register.t
type label = Label.t

type instr =
  | Eint of int32 * register * label
  | Estring of string * register * label (* TODO: cf rtltree *)
  | Ebool of bool * register * label
  | Elea_local of register * int * register * label (* local register | offset | dst *)
  | Elea of register * int * register * label (* src | offset | dst *)
  | Eload of register * int * register * label (* src | offset | dst *)
  | Estore of register * register * int * label (* src | dst | offset *)
  | Ecall of string * int * label (* # args in regs *)
  | Emunop of Istree.munop * register * label (* w/o Mmodir, Mmodil, Midivir *)
  | Embinop of Istree.mbinop * register * register * label
  | Emubranch of Rtltree.mubranch * register * label * label (* true | false *)
  | Embbranch of Rtltree.mbbranch * register * register * label * label (* 2nd arg | 1st arg *)
  | Egoto of label
  | Ealloc_frame of label
  | Efree_frame of label
  | Eget_param of int * register * label
  | Epush_param of register * label
  | Epop_param of register * label
  | Ealloc_stack of int32 * label
  | Efree_stack of int32 * label
  | Ereturn

type decl_fun = {
    formals : int;
    locals : Register.set;
    stored_locals : register list list;
    entry : label;
    body : instr Label.map;
  }

type file = decl_fun Asg.smap

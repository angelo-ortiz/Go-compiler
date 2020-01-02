
(* Explicit register transfer language grammar *)

type register = Register.t
type label = Label.t

type instr =
  | Eint of int32 * register * label
  | Estring of string * register * label (* TODO: cf rtltree *)
  | Ebool of bool * register * label
  | Elea of register * register * label (* src | dst *)
  | Eload of register * int * register * label (* src | offset | dst *)
  | Estore_field of register * register * int * label (* src | dst | offset *)
  | Estore_dref of register * register * label (* src | dst *)
  | Ecall of int * string * int * label (* len(result) | # args in regs *)
  | Emunop of Istree.munop * register * label
  | Embinop of Istree.mbinop * register * register * label
  | Emubranch of Rtltree.mubranch * register * label * label (* true | false *)
  | Embbranch of Rtltree.mbbranch * register * register * label * label (* 2nd arg | 1st arg *)
  | Egoto of label
  | Ealloc_frame of label
  | Efree_frame of label
  | Eget_param of int * register * label
  | Epush_param of register * label
  | Epop_param of register * label
  | Ereturn

type decl_struct = int
  
type decl_fun = {
    formals : int;
    result : int;
    locals : Register.set;
    entry : label;
    body : instr Label.map;
  }

type file = {
    structs : decl_struct Asg.smap;
    functions : decl_fun Asg.smap;
  }

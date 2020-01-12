
(* Register transfer language grammar *)

type register = Register.t
type label = Label.t

type mubranch =
  | Mjz | Mjnz | Mjei of int32 | Mjnei of int32
  | Mjgi of int32 | Mjgei of int32 | Mjli of int32 | Mjlei of int32

type mbbranch =
  | Mje | Mjne | Mjg | Mjge| Mjl | Mjle

type instr =
  | Eint of int32 * register * label
  | Estring of string * register * label
  | Ebool of bool * register * label
  | Emalloc of register * int32 * label
  | Elea_local of register list * int * register * label (* local register | offset | dst *)
  | Elea of register * int * register * label (* src | offset | dst *)
  | Eload of register list * int * register list * label (* src | offset | dst *)
  | Estore of register * register * int * label (* src | dst | offset *)
  | Ecall of register list * string * register list * label (* results | name | args *)
  | Eprint of register list * label (* expressions *)
  | Emunop of Istree.munop * register * label
  | Embinop of Istree.mbinop * register * register * label
  | Emubranch of mubranch * register * label * label (* true | false *)
  | Embbranch of mbbranch * register * register * label * label (* 2nd arg | 1st arg *)
  | Egoto of label

type graph = instr Label.map

type decl_fun = {
    formals : register list;
    result : register list;
    locals : Register.set;
    entry : label;
    exit_ : label;
    body : instr Label.map;
  }

type file = decl_fun Asg.smap

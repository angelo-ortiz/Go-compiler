
(* Register transfer language grammar *)

type register = Register.t
type label = Label.t

type mubranch =
  | Mjz | Mjnz | Mjei of int32 | Mjnei of int32
  | Mjgi of int32 | Mjgei of int32 | Mjli of int32 | Mjlei of int32

type mbbranch =
  | Mje | Mjne | Mjg | Mjge| Mjl | Mjle

type inc_dec =
  | IDinc | IDdec

type instr =
  | Iint of int32 * register * label
  | Istring of string * register * label
  | Ibool of bool * register * label
  | Imalloc of register * int32 * label
  | Ilea_local of register list * int * register * label (* local register | offset | dst *)
  | Ilea of register * int * register * label (* src | offset | dst *)
  | Iload of register list * int * register list * label (* src | offset | dst *)
  | Istore of register * register * int * label (* src | dst | offset *)
  | Icall of register list * string * register list * label (* results | name | args *)
  | Iprint of register list * label (* expressions *)
  | Imunop of Istree.munop * register * label
  (* | Iinc_dec_local of inc_dec * register list * int * label (\* local register | offset *\) *)
  | Iinc_dec of inc_dec * register * int * label (* src/dst | offset *)
  | Imbinop of Istree.mbinop * register * register * label
  | Imubranch of mubranch * register * label * label (* true | false *)
  | Imbbranch of mbbranch * register * register * label * label (* 2nd arg | 1st arg *)
  | Igoto of label

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

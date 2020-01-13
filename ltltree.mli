
(* Location transfer language grammar *)

type register = Register.t
type label = Label.t
type colour = Colouring.colour

type instr =
  | Iint of int32 * colour * label
  | Istring of string * register * label
  | Ibool of bool * colour * label
  | Ilea of register * int * register * label (* src | offset | dst *)
  | Iload of register * int * register * label (* src | offset | dst *)
  | Istore of register * register * int * label (* src | dst | offset *)
  | Icall of string * label
  | Imunop of Ertltree.munop * colour * label
  | Iidiv_imm of int32 * label
  | Iidiv of colour * label
  | Iinc_dec of Rtltree.inc_dec * register * int * label (* src | offset *)
  | Imbinop of Ertltree.mbinop * colour * colour * label
  | Imubranch of Rtltree.mubranch * colour * label * label (* true | false *)
  | Imbbranch of Rtltree.mbbranch * colour * colour * label * label (* 2nd arg | 1st arg *)
  | Igoto of label
  | Ipush of colour * label
  | Ipop of register * label
  | Ireturn

type frame = {
    f_params : int; (* size of return address *)
    f_locals : int; (* size of local variables + used calle-saved registers *)
  }

type decl_fun = {
    entry : label;
    body : instr Label.map;
  }

type file = decl_fun Asg.smap

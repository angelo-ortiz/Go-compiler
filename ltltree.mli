
(* Location transfer language grammar *)

type register = Register.t
type label = Label.t
type colour = Colouring.colour

type instr =
  | Eint of int32 * colour * label
  | Estring of string * colour * label (* TODO: cf rtltree *)
  | Ebool of bool * colour * label
  | Elea of register * int * register * label (* src | offset | dst *)
  | Eload of register * int * register * label (* src | offset | dst *)
  | Estore of register * register * int * label (* src | dst | offset *)
  | Ecall of string * int * label (* # args in regs *)
  | Emunop of Istree.munop * colour * label
  | Embinop of Istree.mbinop * colour * colour * label
  | Emubranch of Rtltree.mubranch * colour * label * label (* true | false *)
  | Embbranch of Rtltree.mbbranch * colour * colour * label * label (* 2nd arg | 1st arg *)
  | Egoto of label
  | Epush of colour * label
  | Epop of register * label
  | Ereturn

type frame = {
    f_params : int; (* size of return address *)
    f_locals : int; (* size of local variables + used calle-saved registers *)
  }

type decl_fun = {
    entry : label;
    body : instr Label.map;
  }

type file = decl_fun Asg.smap

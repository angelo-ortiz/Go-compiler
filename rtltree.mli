
(* Register transfer language grammar *)

open Istree

type register = Register.t
type label = Label.t

type mubranch =
  | Mjz | Mjnz | Mjei of int64 | Mjnei of int64
  | Mjgi of int64 | Mjgei of int64 | Mjli of int64 | Mjlei of int64

type mbbranch =
  | Mje | Mjne | Mjl | Mjle | Mjg | Mjge
           
type instr =
  | Eint of int64 * register * label
  | Estring of string * register * label (* TODO *)
  | Ebool of bool * register * label
  | Enew of register * int * label
  | Eload of register * int * register * label (* src | offset | dst *)
  | Estore of register * register * int * label (* src | dst | offset *)
  | Ecall of register list * string * register list * label (* result | name | args *)
  | Eprint of register list * label (* expressions *)
  | Emunop of munop * register * label
  | Embinop of mbinop * register * register * label
  | Emubranch of mubranch * register * label * label (* true | false *)
  | Embbranch of mbbranch * register * register * label * label (* 2nd arg | 1st arg *)
  | Egoto of label

type graph = instr Label.map

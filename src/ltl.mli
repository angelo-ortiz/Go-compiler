
(* Location transfer language grammar *)

open Colouring

type linstr =
  | Iint of int64 * colour * Label.t
  | Istring of string * Register.t * Label.t
  | Ibool of bool * colour * Label.t
  | Ilea of Register.t * int * Register.t * Label.t (* src | offset | dst *)
  | Iload of Register.t * int * Register.t * Label.t (* src | offset | dst *)
  | Istore of Register.t * Register.t * int * Label.t (* src | dst | offset *)
  | Icall of string * Label.t
  | Imunop of Ertl.emunop * colour * Label.t
  | Iidiv_imm of int64 * Label.t
  | Iidiv of colour * Label.t
  | Iinc_dec of Rtl.inc_dec * Register.t * int * Label.t (* src | offset *)
  | Imbinop of Ertl.embinop * colour * colour * Label.t
  | Imubranch of Rtl.mubranch * colour * Label.t * Label.t (* true | false *)
  | Imbbranch of Rtl.mbbranch * colour * colour * Label.t * Label.t (* 2nd arg | 1st arg *)
  | Igoto of Label.t
  | Ipush of colour * Label.t
  | Ipop of Register.t * Label.t
  | Ireturn

type frame = {
    f_params : int; (* size of return address *)
    f_locals : int; (* size of local variables + used calle-saved registers *)
  }

type lfundef = {
    entry : Label.t;
    body : linstr Label.map;
  }

type lprogramme = lfundef Utils.smap

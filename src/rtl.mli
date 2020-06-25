
(* Register transfer language grammar *)

type cell =
  | Format of string
  | Type of Asg.t_typ

type mubranch =
  | Mjz | Mjnz | Mjei of int64 | Mjnei of int64
  | Mjgi of int64 | Mjgei of int64 | Mjli of int64 | Mjlei of int64

type mbbranch =
  | Mje | Mjne | Mjg | Mjge| Mjl | Mjle

type inc_dec =
  | IDinc | IDdec

type rinstr =
  | Iint of int64 * Register.t * Label.t
  | Istring of string * Register.t * Label.t
  | Ibool of bool * Register.t * Label.t
  | Imalloc of Register.t * int32 * Label.t
  | Ilea_local of Register.t list * int * Register.t * Label.t (* local Register.t | offset | dst *)
  | Ilea of Register.t * int * Register.t * Label.t (* src | offset | dst *)
  | Iload of Register.t * int * Register.t * Label.t (* src | offset | dst *)
  | Istore of Register.t * Register.t * int * Label.t (* src | dst | offset *)
  | Icall of Register.t list * string * Register.t list * Label.t (* results | name | args *)
  | Iprint of Register.t list * Label.t (* expressions *)
  | Imunop of Isl.munop * Register.t * Label.t
  (* | Iinc_dec_local of inc_dec * Register.t list * int * Label.t (\* local Register.t | offset *\) *)
  | Iinc_dec of inc_dec * Register.t * int * Label.t (* src/dst | offset *)
  | Imbinop of Isl.mbinop * Register.t * Register.t * Label.t
  | Imubranch of mubranch * Register.t * Label.t * Label.t (* true | false *)
  | Imbbranch of mbbranch * Register.t * Register.t * Label.t * Label.t (* 2nd arg | 1st arg *)
  | Igoto of Label.t

type graph = rinstr Label.map

type rfundef = {
    formals : Register.t list;
    result : Register.t list;
    locals : Register.set;
    entry : Label.t;
    exit_ : Label.t;
    body : rinstr Label.map;
  }

type rprogramme = {
    structs : Isl.istrdef Utils.smap;
    functions : rfundef Utils.smap;
  }


open Register
open Interference
open Colouring

let pp_colour fmt v = function
  | Reg r ->
     Format.fprintf fmt "\n\t%a -> reg %a" string_of_reg v string_of_reg r
  | Spilt n ->
     Format.fprintf fmt "\n\t%a -> spilt %d" string_of_reg v n
  | Heap (s, h) ->
     Format.fprintf fmt "\n\t%a -> heap (s:%d, h:%d)" string_of_reg v s h
    

let rec pp_set fmt set =
  if Register.S.is_empty set then ()
  else
    let r = Register.S.choose set in
    let set = Register.S.remove r set in
    Format.fprintf fmt "%a, %a" string_of_reg r pp_set set
    
let pp_graph fmt v arcs =
  Format.fprintf fmt "\t@[%a: prefs = {%a}, intfs = {%a}@]@."
    Register.string_of_reg v pp_set arcs.prefs
  pp_set arcs.intfs
    
let pp fmt pp_fct =
  Register.M.iter (pp_fct fmt)

let pp_c fmt = pp fmt pp_colour

let pp_g fmt = pp fmt pp_graph


type t = string

module S = Set.Make(String)
type set = S.t
         
module M = Map.Make(String)
type 'a map = 'a M.t

let counter = ref 0

let fresh () =
  incr counter;
  Format.sprintf "._L%d" !counter

let to_string l =
  l

let string_of_label fmt l =
  Format.fprintf fmt "%s" l

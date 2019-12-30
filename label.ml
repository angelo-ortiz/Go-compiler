
type t = string

let counter = ref 0

let fresh () =
  incr counter;
  Format.sprintf ".L%d" !counter

module M = Map.Make(String)

type 'a map = 'a M.t

module S = Set.Make(String)

type set = S.t

let string_of_label fmt l =
  Format.fprintf fmt "%s" l

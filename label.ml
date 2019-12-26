
type t = string

let counter = ref 0

let fresh () =
  incr counter;
  Format.sprintf "L%d" !counter

module M = Map.Make(String)

type 'a map = 'a M.t

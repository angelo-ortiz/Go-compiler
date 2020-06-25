
open Ltl
open UnionFind
   
let compress entry graph =

  let elements =
    Label.M.mapi (fun l _ ->
        UnionFind.fresh l
      ) graph
  in
  
  let lookup l =
    Label.M.find l elements
  in
  
  Label.M.iter (fun l i ->
      let elt = lookup l in
      match i with
      | Igoto next_l ->
         let next_elt = lookup next_l in
         UnionFind.union elt next_elt
      | _ ->
         ()
    ) graph;
  
  let repr l =
    UnionFind.contents (lookup l)
  in
  
  let remove_goto = function
    | Iint (n, c, l) ->
       Iint (n, c, repr l)
    | Istring (s, c, l) ->
       Istring (s, c, repr l)
    | Ibool (b, c, l) ->
       Ibool (b, c, repr l)
    | Ilea (src, ofs, dst, l) ->
       Ilea (src, ofs, dst, repr l)
    | Iload (src, ofs, dst, l) ->
       Iload (src, ofs, dst, repr l)
    | Istore (src, dst, ofs, l) ->
       Istore (src, dst, ofs, repr l)
    | Icall (f, l) ->
       Icall (f, repr l)
    | Imunop (op, c, l) ->
       Imunop (op, c, repr l)
    | Iidiv_imm (n, l) ->
       Iidiv_imm (n, repr l)
    | Iidiv (c, l) ->
       Iidiv (c, repr l)
    | Iinc_dec (op, r, ofs, l) ->
       Iinc_dec (op, r, ofs, repr l)
    | Imbinop (op, c1, c2, l) ->
       Imbinop (op, c1, c2, repr l)
    | Imubranch (op, c, true_l, false_l) ->
       Imubranch (op, c, repr true_l, repr false_l)
    | Imbbranch (op, c1, c2, true_l, false_l) ->
       Imbbranch (op, c1, c2, repr true_l, repr false_l)
    | Igoto l ->
       Igoto (repr l)
    | Ipush (c, l) ->
       Ipush (c, repr l)
    | Ipop (r, l) ->
       Ipop (r, repr l)
    | Ireturn ->
       Ireturn
  in
  
  repr entry, Label.M.map remove_goto graph

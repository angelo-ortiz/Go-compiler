
open Istree
open Rtltree

let graph = ref Label.M.empty

let locals = ref (Hashtbl.create 32)
let functions = ref []

let branch_of_unop = function
  | Istree.Mjz -> Rtltree.Mjz
  | Istree.Mjnz -> Rtltree.Mjnz
  | Istree.Mjei n -> Rtltree.Mjnei 

let multi_fresh_list =
  List.map (fun _ -> Register.fresh ())
  
let multi_fresh n =
  let rec loop i acc =
    if i = 0 then acc
    else loop (i-1) (Register.fresh () :: acc)
  in
  loop n []
              
let generate a =
  let l = Label.fresh () in
  graph := Label.M.add l a !graph;
  l

let rec expr destr e destl =
  match e with
  | IEint n ->
     generate (Eint (n, destr, destl))
  | IEstring s ->
     generate (Estring (s, destr, destl))
  | IEbool b ->
     generate (Ebool (b, destr, destl))
  | IEnil ->
    generate (Eint (0L, destr, destl))
  | IEnew ty ->
     assert false
  (* type or list of registers??? *)
  (* generate (Enew (destr, ty, destl)) *)
  | IEaccess v ->
     let rx = Hashtbl.find !locals v in
     generate (Embinop (Mmov, rx, destr, destl))
  | IEload (s, f) ->
     let rx = Register.fresh () in
     expr rx s (
         generate (Eload (rx, f, destr, destl))
       )
  | IEcall (f, actuals) ->
     let r_args = multi_fresh_list actuals in
     let r_res = multi_fresh (List.assoc f !functions) in
     let lab = generate (Ecall (r_res, f, r_args, destl)) in
     List.fold_right2 expr r_args actuals lab
  | IEprint es ->
     let regs = multi_fresh_list es in
     let lab = generate (Eprint (regs, destl)) in
     List.fold_right2 expr regs es lab
  | IEunop (op, e) ->
     expr destr e (
         generate (Emunop (op, destr, destl))
       )
  | IEbinop (op, e1, e2) ->
     let rx = Register.fresh () in
     expr destr e1 (
         expr rx e2 (
             generate (Embinop (op, rx, destr, destl))
       ))
  | _ ->
     assert false

let rec condition e true_l false_l =
  match e with
  | IEbool b ->
     generate (Egoto (if b then true_l else false_l))
  | IEand (e1, e2) ->
     condition e1 (condition e2 true_l false_l) false_l
  | IEor (e1, e2) ->
     condition e1 true_l (condition e2 true_l false_l)
  | IEunop (Mjz, e) -> (* <> 0 is more likely than = 0 *)
     let tmp = Register.fresh () in
     expr tmp e (
         generate (Emubranch (Mjnz, tmp, false_l, true_l))
       )
  | IEunop (Mjnz, e) ->
     let tmp = Register.fresh () in
     expr tmp e (
         generate (Emubranch (Mjnz, tmp, true_l, false_l))
       )
  | IEunop (Mjei n, e) -> (* <> n is more likely than = n *)
     let tmp = Register.fresh () in
     expr tmp e (
         generate (Emubranch (Mjnei n, tmp, false_l, true_l))
       )
  | IEunop (Mjnei n | Mjgi n | Mjgei n | Mjli n | Mjlei n as op, e) ->
     let tmp = Register.fresh () in
     expr tmp e (
         generate (Emubranch (op, tmp, true_l, false_l))
       )
  | IEunop (Mjgi n, e) ->
     let tmp = Register.fresh () in
     expr tmp e (
         generate (Emubranch (Mjgi n, tmp, true_l, false_l))
       )
  | _ ->
     assert false (* TODO *)

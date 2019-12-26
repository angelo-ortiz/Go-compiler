
open Istree
open Rtltree

let graph = ref Label.M.empty
let locals = ref (Hashtbl.create 32)
let result_number = ref []

let multi_fresh_list l =
  List.map (fun _ -> Register.fresh ()) l
  
let multi_fresh_int n =
  let rec loop i acc =
    if i = 0 then acc
    else loop (i-1) (Register.fresh () :: acc)
  in
  loop n []
              
let generate a =
  let l = Label.fresh () in
  graph := Label.M.add l a !graph;
  l

let branch_of_unop = function
  | Istree.Msetei n ->
     Rtltree.Mjei n
  | Istree.Msetnei n ->
     Rtltree.Mjnei n
  | Istree.Msetgi n ->
     Rtltree.Mjgi n
  | Istree.Msetgei n ->
     Rtltree.Mjgei n
  | Istree.Msetli n ->
     Rtltree.Mjli n
  | Istree.Msetlei n ->
     Rtltree.Mjlei n
  | _ ->
     assert false

let branch_of_binop = function
  | Istree.Msete ->
     Rtltree.Mje
  | Istree.Msetne ->
     Rtltree.Mjne
  | Istree.Msetg ->
     Rtltree.Mjg
  | Istree.Msetge ->
     Rtltree.Mjge
  | Istree.Msetl ->
     Rtltree.Mjl
  | Istree.Msetle ->
     Rtltree.Mjle
  | _ ->
     assert false

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
  | IEmalloc n ->
     generate (Emalloc (destr, n, destl))
  | IEaccess v ->
     let rx = Hashtbl.find !locals v in
     generate (Embinop (Mmov, rx, destr, destl))
  | IEload (str, n) ->
     let rx = Register.fresh () in
     expr rx str (
         generate (Eload (rx, n, destr, destl))
       )
  | IEcall (f, actuals) ->
     let r_args = multi_fresh_list actuals in
     let r_res = multi_fresh_int (List.assoc f !result_number) in
     let lab = generate (Ecall (r_res, f, r_args, destl)) in
     List.fold_right2 expr r_args actuals lab
  | IEprint es ->
     let regs = multi_fresh_list es in
     let lab = generate (Eprint (regs, destl)) in (* TODO: reg list -> reg *)
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
  | IEand  _ | IEor _ ->
     assert false

let rec condition e true_l false_l =
  match e with
  | IEbool b ->
     generate (Egoto (if b then true_l else false_l))
  | IEand (e1, e2) ->
     condition e1 (condition e2 true_l false_l) false_l
  | IEor (e1, e2) ->
     condition e1 true_l (condition e2 true_l false_l)
  | IEunop (Msetei n, e) -> (* <> n is more likely than = n *)
     let tmp = Register.fresh () in
     expr tmp e (
         generate (
             let op = if n = 0L then Mjnz else Mjnei n in
             Emubranch (op, tmp, false_l, true_l)
           )
       )
  | IEunop (Msetnei n, e) ->
     let tmp = Register.fresh () in
     expr tmp e (
         generate (
             let op = if n = 0L then Mjnz else Mjnei n in
             Emubranch (op, tmp, true_l, false_l)
           )
       )
  | IEunop (Msetgi n | Msetgei n | Msetli n | Msetlei n as op, e) ->
     let tmp = Register.fresh () in
     expr tmp e (
         generate (Emubranch (branch_of_unop op, tmp, true_l, false_l))
       )
  | IEbinop (op, e1, e2) ->
     let tmp1 = Register.fresh () in
     let tmp2 = Register.fresh () in
     expr tmp1 e1 (
         expr tmp2 e2 (
             generate (Embbranch (branch_of_binop op, tmp2, tmp1, true_l, false_l))
       ))
  | _ ->
     assert false

let rec stmt retrs s exitl destl =
  match s with
  | ISexpr e -> (* incr and decr only *)
     let tmp = Register.fresh () in
     expr tmp e destl
  | IScall (f, actuals) ->
     (* tmp is actually not used *)
     let tmp = Register.fresh () in
     expr tmp (IEcall (f, actuals)) destl
  | ISprint es ->
     (* tmp is actually not used *)
     let tmp = Register.fresh () in
     expr tmp (IEprint es) destl
  | ISblock b ->
     block retrs b exitl destl
  | ISif (e, s1, s2) ->
     condition e
       (block retrs s1 exitl destl)
       (block retrs s2 exitl destl)
  | ISassign (vars, values) ->
     (* TODO: underscore? diff btw pointer, struct-field, var?*)
     
     (* let r_args = multi_fresh_list actuals in
      * let r_res = multi_fresh (List.assoc f !functions) in
      * let lab = generate (Ecall (r_res, f, r_args, destl)) in
      * List.fold_right2 expr r_args actuals lab *)
     assert false
  | ISreturn es ->
     List.fold_right2 expr retrs es exitl
  | ISfor (e, b) ->
     let l = Label.fresh () in
     let entry = condition e (stmt retrs s exitl l) destl in
     graph := Label.M.add l (Egoto entry) !graph;
     entry

and block retrs b exitl destl =
  List.fold_right (fun st dlab -> stmt retrs st exitl dlab) b destl

let funct (f:Istree.idecl_fun) =
  let formals = multi_fresh_list f.formals in
  List.iter2 (fun v r -> Hashtbl.add !locals v r) f.formals formals; 
  let result = multi_fresh_int f.result in
  let local_vars =
    List.fold_left
      (fun set v ->
        let reg = Register.fresh () in
        Hashtbl.add !locals v reg;
        Register.S.add reg set 
      ) Register.S.empty f.locals
  in
  let exit_ = Label.fresh () in
  let entry = block result f.body exit_ exit_ in
  let body = !graph in
  locals := Hashtbl.create 32;
  graph := Label.M.empty;
  { formals; result; locals = local_vars; entry; exit_; body } 
  
let file (f:Istree.ifile) =
  let add_retrs f (decl:Istree.idecl_fun) acc =
    (f, decl.result) :: acc
  in
  result_number := Asg.Smap.fold add_retrs f.functions []; 
  let functions = Asg.Smap.map funct f.functions in
  { structs = f.structs; functions }

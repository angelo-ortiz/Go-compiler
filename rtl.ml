
open Istree
open Rtltree

let graph = ref Label.M.empty
let locals = ref (Hashtbl.create 32)
let number_formals_results = ref []

let listify =
  fun x -> [x]
                           
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

let rec expr destrs e destl =
  match e with
  | IEint n ->
     generate (Eint (n, List.hd destrs, destl))
  | IEstring s ->
     generate (Estring (s, List.hd destrs, destl))
  | IEbool b ->
     generate (Ebool (b, List.hd destrs, destl))
  | IEnil ->
    generate (Eint (0l, List.hd destrs, destl))
  | IEmalloc n ->
     generate (Emalloc (List.hd destrs, n, destl))
  | IEaccess v ->
     let rx = Hashtbl.find !locals v in
     generate (Embinop (Mmov, rx, List.hd destrs, destl))
  | IEload (str, n) ->
     let rx = Register.fresh () in
     expr [rx] str (
         generate (Eload (rx, n, List.hd destrs, destl))
       )
  | IEcall (f, actuals) ->
     let n_formals, _ = List.assoc f !number_formals_results in
     let r_args = multi_fresh_int n_formals in
     let lab = generate (Ecall (destrs, f, r_args, destl)) in
     if List.length actuals < n_formals then (* actuals is a single function call *)
      expr r_args (List.hd actuals) lab
     else (* one actual parameter per formal one *)
       List.fold_right2 expr (List.map listify r_args) actuals lab
  | IEunop (Mdref, e) ->
     let rx = List.hd destrs in
     expr destrs e (
         generate (Eload (rx, 0, rx, destl))
       )
  | IEunop (Maddr, e) ->
     let tmp = Register.fresh () in
     expr [tmp] e (
         generate (Elea (tmp, List.hd destrs, destl))
       )
  | IEunop (op, e) ->
     expr destrs e (
         generate (Emunop (op, List.hd destrs, destl))
       )
  | IEbinop (op, e1, e2) ->
     let tmp = Register.fresh () in
     expr destrs e1 (
         expr [tmp] e2 (
             generate (Embinop (op, tmp, List.hd destrs, destl))
       ))
  | IEand (e1, e2) ->
     let true_l = expr destrs e2 destl  in
     let false_l = generate (Ebool (false, List.hd destrs, destl)) in
     condition e1 true_l false_l
  | IEor (e1, e2) ->
     let true_l = generate (Ebool (true, List.hd destrs, destl))  in
     let false_l = expr destrs e2 destl in
     condition e1 true_l false_l

and condition e true_l false_l =
  match e with
  | IEbool b ->
     generate (Egoto (if b then true_l else false_l))
  | IEand (e1, e2) ->
     condition e1 (condition e2 true_l false_l) false_l
  | IEor (e1, e2) ->
     condition e1 true_l (condition e2 true_l false_l)
  | IEunop (Msetei n, e) -> (* <> n is more likely than = n *)
     let tmp = Register.fresh () in
     expr [tmp] e (
         generate (
             let op = if n = 0l then Mjnz else Mjnei n in
             Emubranch (op, tmp, false_l, true_l)
           )
       )
  | IEunop (Msetnei n, e) ->
     let tmp = Register.fresh () in
     expr [tmp] e (
         generate (
             let op = if n = 0l then Mjnz else Mjnei n in
             Emubranch (op, tmp, true_l, false_l)
           )
       )
  | IEunop (Msetgi n | Msetgei n | Msetli n | Msetlei n as op, e) ->
     let tmp = Register.fresh () in
     expr [tmp] e (
         generate (Emubranch (branch_of_unop op, tmp, true_l, false_l))
       )
  | IEunop (Mnot, e) ->
     condition e false_l true_l
  | IEbinop (Msete | Msetne | Msetg | Msetge | Msetl | Msetle as op, e1, e2) ->
     let tmp1 = Register.fresh () in
     let tmp2 = Register.fresh () in
     expr [tmp1] e1 (
         expr [tmp2] e2 (
             generate (Embbranch (branch_of_binop op, tmp2, tmp1, true_l, false_l))
       ))
  | e ->
     let tmp = Register.fresh () in
     expr [tmp] e (
         generate (Emubranch (Mjz, tmp, false_l, true_l))
       )

let assign srcr e destl =
  match e with
  | Avar v ->
     let dstr = Hashtbl.find !locals v in
     generate (Embinop (Mmov, srcr, dstr, destl))
  | Afield (str, n) ->
     let dstr = Register.fresh () in
     expr [dstr] str (
         generate (Estore_field (srcr, dstr, n, destl))
       )
  | Adref e ->
     let dstr = Register.fresh () in
     expr [dstr] e (
         generate (Estore_dref (srcr, dstr, destl))
       )
     
let rec stmt retrs s exitl destl =
  match s with
  | ISexpr e -> (* incr and decr only *)
     let tmp = Register.fresh () in
     expr [tmp] e destl
  | IScall (f, actuals) ->
     let _, n_results = List.assoc f !number_formals_results in
     let destrs = multi_fresh_int n_results in
     expr destrs (IEcall (f, actuals)) destl
  | ISprint (fmt, es) ->
     let fmt_reg = Register.fresh () in
     let destrs = multi_fresh_list es in
     let l = generate (Eprint (fmt_reg :: destrs, destl)) in
     let l = List.fold_right2 expr (List.map listify destrs) es l in
     generate (Estring (fmt, fmt_reg, l)) 
  | ISif (e, bif, belse) ->
     condition e
       (block retrs bif exitl destl)
       (block retrs belse exitl destl)
  | ISassign (vars, [IEcall (f, actuals) as e]) ->
     let srcrs = multi_fresh_list vars in
     let l = List.fold_right2 assign srcrs vars destl in
     expr srcrs e l
  | ISassign (vars, values) ->
     let srcrs = multi_fresh_list vars in
     let l = List.fold_right2 assign srcrs vars destl in
     List.fold_right2 expr (List.map listify srcrs) values l
  | ISreturn [IEcall (f, actuals) as e] ->
     expr retrs e exitl
  | ISreturn es ->
     List.fold_right2 expr (List.map listify retrs) es exitl
  | ISfor (e, bfor) ->
     let l = Label.fresh () in
     let entry = condition e (block retrs bfor exitl l) destl in
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
    (f, (List.length decl.formals, decl.result)) :: acc
  in
  number_formals_results := Asg.Smap.fold add_retrs f.functions []; 
  let functions = Asg.Smap.map funct f.functions in
  { structs = f.structs; functions }


open Istree
open Rtltree

let graph = ref Label.M.empty
let locals = Hashtbl.create 32
let number_formals_results = ref []

let listify =
  fun x -> [x]

let reg_of_option = function
  | None ->
     assert false
  | Some r ->
     r
         
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
  match e.desc with
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
     let rxs = Hashtbl.find locals v in
     List.fold_right2 (fun src dst l -> generate (Embinop (Mmov, src, dst, l))) rxs destrs destl
  | IEselect (str, n) ->
     let tmps = multi_fresh_int str.length in
     let srcs = Utils.sub_list tmps n e.length in
     let l = List.fold_right2 (fun src dst l ->
                 generate (Embinop (Mmov, src, dst, l))
               ) srcs destrs destl
     in
     expr tmps str l
  | IEload (str, n) ->
     let tmps = multi_fresh_int e.length in
     expr tmps str (generate (
     Eload (tmps, n, destrs, destl)))
  | IEcall (f, actuals) ->
     let n_formals, n_results = List.assoc f !number_formals_results in
     let r_args = List.map multi_fresh_int n_formals in
     let f_args = List.flatten r_args in
     let lab = generate (Ecall (destrs, f, f_args, destl)) in
     if List.length actuals < Utils.sum_of_list n_formals then (* actuals is a single function call *)
      expr f_args (List.hd actuals) lab
     else (* one actual parameter per formal one *)
       List.fold_right2 expr r_args actuals lab
  | IEaddr { length; desc = IEaccess v } ->
     let rxs = Hashtbl.find locals v in
     generate (Elea_local (rxs, 0, List.hd destrs, destl))
  | IEaddr { length; desc = IEselect (str, n) } ->
     let rxs = multi_fresh_int str.length in
     expr rxs str (generate (
     Elea_local (rxs, n, List.hd destrs, destl)))
  | IEaddr { length; desc = IEload (str, n) } ->
     let tmps = multi_fresh_int str.length in
     expr tmps str (generate (
     Elea (List.hd tmps, n, List.hd destrs, destl)))
  | IEaddr _ -> (* TODO: Really unused??? *)
     assert false
  | IEunop (op, e) ->
     expr destrs e (generate (
     Emunop (op, List.hd destrs, destl)))
  | IEbinop (Msete|Msetne as op, e1, e2) when e1.length > 1 ->
     let tmps1 = multi_fresh_int e1.length in
     let tmps2 = multi_fresh_int e2.length in
     let dst = List.hd destrs in
     let true_l = generate (Ebool (true, dst, destl)) in 
     let false_l = generate (Ebool (false, dst, destl)) in
     let cont, break = if op = Msete then  true_l, false_l else false_l, true_l in
     let l = List.fold_right2 (fun r1 r2 cont -> 
                 generate (Embbranch (Mjne, r2, r1, break, cont))
               ) tmps1 tmps2 cont
     in
     expr tmps1 e1 (expr tmps2 e2 l)
  | IEbinop (op, e1, e2) ->
     let tmp = Register.fresh () in
     expr destrs e1 (
     expr [tmp] e2 (generate (
     Embinop (op, tmp, List.hd destrs, destl))))
  | IEand (e1, e2) ->
     let true_l = expr destrs e2 destl  in
     let false_l = generate (Ebool (false, List.hd destrs, destl)) in
     condition e1 true_l false_l
  | IEor (e1, e2) ->
     let true_l = generate (Ebool (true, List.hd destrs, destl))  in
     let false_l = expr destrs e2 destl in
     condition e1 true_l false_l

and condition e true_l false_l =
  match e.desc with
  | IEbool b ->
     generate (Egoto (if b then true_l else false_l))
  | IEand (e1, e2) ->
     condition e1 (condition e2 true_l false_l) false_l
  | IEor (e1, e2) ->
     condition e1 true_l (condition e2 true_l false_l)
  | IEunop (Msetei n, e) -> (* <> n is more likely than = n *)
     let tmp = Register.fresh () in
     expr [tmp] e (generate (
     let op = if n = 0l then Mjnz else Mjnei n in
     Emubranch (op, tmp, false_l, true_l)))
  | IEunop (Msetnei n, e) ->
     let tmp = Register.fresh () in
     expr [tmp] e (generate (
     let op = if n = 0l then Mjnz else Mjnei n in
     Emubranch (op, tmp, true_l, false_l)))
  | IEunop (Msetgi n | Msetgei n | Msetli n | Msetlei n as op, e) ->
     let tmp = Register.fresh () in
     expr [tmp] e (generate (
     Emubranch (branch_of_unop op, tmp, true_l, false_l)))
  | IEunop (Mnot, e) ->
     condition e false_l true_l
  | IEbinop (Msete|Msetne as op, e1, e2) ->
     let tmps1 = multi_fresh_int e1.length in
     let tmps2 = multi_fresh_int e2.length in
     let cont, break = if op = Msete then  true_l, false_l else false_l, true_l in
     let l = List.fold_right2 (fun r1 r2 cont -> 
                 generate (Embbranch (Mjne, r2, r1, break, cont))
               ) tmps1 tmps2 cont
     in
     expr tmps1 e1 (expr tmps2 e2 l)
  | IEbinop (Msetg | Msetge | Msetl | Msetle as op, e1, e2) ->
     let tmp1 = Register.fresh () in
     let tmp2 = Register.fresh () in
     expr [tmp1] e1 (
     expr [tmp2] e2 (generate (
     Embbranch (branch_of_binop op, tmp2, tmp1, true_l, false_l))))
  | _ ->
     let tmp = Register.fresh () in
     expr [tmp] e (
         generate (Emubranch (Mjz, tmp, false_l, true_l))
       )

let assign srcrs e destl =
  match e.assignee with
  | Avar v ->
     let dstrs = Hashtbl.find locals v in
     List.fold_right2 (fun src dst l -> generate (Embinop (Mmov, src, dst, l))) srcrs dstrs destl
  | Afield (str, n) ->
     let tmps = multi_fresh_int str.length in
     let dstrs = Utils.sub_list tmps n e.length in
     let l = List.fold_right2 (fun src dst l ->
                 generate (Embinop (Mmov, src, dst, l))
               ) srcrs dstrs destl
     in
     expr tmps str l
  | Adref (e, n) ->
     let dstr = Register.fresh () in
     let l, _ = List.fold_left (fun (l, n) srcr ->
                 generate (Estore (srcr, dstr, n, l)), n + Utils.word_size
               ) (destl, n) srcrs
     in 
     expr [dstr] e l
     
let rec stmt retrs s exitl destl =
  match s with
  | ISexpr e -> (* incr and decr only *)
     let tmp = Register.fresh () in
     expr [tmp] e destl
  | IScall (f, actuals) ->
     let _, l_results = List.assoc f !number_formals_results in
     let n_results = Utils.sum_of_list l_results in
     let destrs = multi_fresh_int n_results in
     expr destrs { length = n_results; desc = IEcall (f, actuals) } destl
  | ISprint (fmt, es) -> (* es: list of 8-byte fit expressions *)
     let fmt_reg = Register.fresh () in
     let destrs = multi_fresh_list es in
     let l = generate (Eprint (fmt_reg :: destrs, destl)) in
     let l = List.fold_right2 expr (List.map listify destrs) es l in
     generate (Estring (fmt, fmt_reg, l)) 
  | ISif (e, bif, belse) ->
     condition e
       (block retrs bif exitl destl)
       (block retrs belse exitl destl)
  | ISassign (vars, [{ length; desc = IEcall (f, actuals) } as e]) ->
     let srcrs = List.map (fun v -> multi_fresh_int v.length) vars in
     let l = List.fold_right2 assign srcrs vars destl in
     expr (List.flatten srcrs) e l
  | ISassign (vars, values) ->
     let srcrs = List.map (fun (v:Istree.iexpr) -> multi_fresh_int v.length) values in
     let l = List.fold_right2 assign srcrs vars destl in
     List.fold_right2 expr srcrs values l
  | ISreturn [{ length; desc = IEcall (f, actuals) } as e] ->
     expr (List.hd retrs) e exitl
  | ISreturn es ->
     List.fold_right2 expr retrs es exitl
  | ISfor (e, bfor) ->
     let l = Label.fresh () in
     let entry = condition e (block retrs bfor exitl l) destl in
     graph := Label.M.add l (Egoto entry) !graph;
     entry

and block retr b exitl destl =
  List.fold_right (fun st dlab -> stmt retr st exitl dlab) b destl
  
let funct (f:Istree.idecl_fun) =
  let r_formals = List.map (fun (_, n) -> multi_fresh_int n) f.formals in
  List.iter2 (fun (v, _) rs -> Hashtbl.add locals v rs) f.formals r_formals; 
  let result = List.map (fun len -> multi_fresh_int len) f.result in
  let local_vars =
    List.fold_left
      (fun set (v, n) ->
        let regs = multi_fresh_int n in
        Hashtbl.add locals v regs;
        List.fold_left (fun set r -> Register.S.add r set ) set regs
      ) Register.S.empty f.locals
  in
  let exit_ = Label.fresh () in
  let entry = block result f.body exit_ exit_ in
  let body = !graph in
  Hashtbl.reset locals;
  graph := Label.M.empty;
  { formals = List.flatten r_formals; result = List.flatten result;
    locals = local_vars; entry; exit_; body } 
  
let file (f:Istree.ifile) =
  let add_retrs f (decl:Istree.idecl_fun) acc =
    (f, (List.map snd decl.formals, decl.result)) :: acc
  in
  number_formals_results := Asg.Smap.fold add_retrs f []; 
  Asg.Smap.map funct f

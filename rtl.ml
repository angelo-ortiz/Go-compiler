
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
     generate (Iint (n, List.hd destrs, destl))
  | IEstring s ->
     generate (Istring (s, List.hd destrs, destl))
  | IEbool b ->
     generate (Ibool (b, List.hd destrs, destl))
  | IEnil ->
    generate (Iint (0l, List.hd destrs, destl))
  | IEmalloc n ->
     generate (Imalloc (List.hd destrs, n, destl))
  | IEaccess v when v = "_" ->
     let rxs = multi_fresh_int e.length in
     List.fold_right2 (fun src dst l -> generate (Imbinop (Mmov, src, dst, l))) rxs destrs destl
  | IEaccess v ->
     let rxs = Hashtbl.find locals v in
     List.fold_right2 (fun src dst l -> generate (Imbinop (Mmov, src, dst, l))) rxs destrs destl
  | IEselect (str, n) ->
     let tmps = multi_fresh_int str.length in
     let srcs = Utils.sub_list tmps n e.length in
     let l = List.fold_right2 (fun src dst l ->
                 generate (Imbinop (Mmov, src, dst, l))
               ) srcs destrs destl
     in
     expr tmps str l
  | IEload (str, n) ->
     let tmps = multi_fresh_int e.length in
     expr tmps str (generate (
     Iload (tmps, n, destrs, destl)))
  | IEcall (f, actuals) ->
     let n_formals, n_results = List.assoc f !number_formals_results in
     let r_args = List.map multi_fresh_int n_formals in
     let f_args = List.flatten r_args in
     let lab = generate (Icall (destrs, f, f_args, destl)) in
     if List.length actuals < Utils.sum_of_list n_formals then (* actuals is a single function call *)
      expr f_args (List.hd actuals) lab
     else (* one actual parameter per formal one *)
       List.fold_right2 expr r_args actuals lab
  | IEaddr { length; desc = IEaccess v } ->
     let rxs = Hashtbl.find locals v in
     generate (Ilea_local (rxs, 0, List.hd destrs, destl))
  | IEaddr { length; desc = IEselect (str, n) } ->
     let rxs = multi_fresh_int str.length in
     expr rxs str (generate (
     Ilea_local (rxs, n, List.hd destrs, destl)))
  | IEaddr { length; desc = IEload (str, n) } ->
     let tmps = multi_fresh_int str.length in
     expr tmps str (generate (
     Ilea (List.hd tmps, n, List.hd destrs, destl)))
  | IEaddr _ -> (* TODO: Really unused??? *)
     assert false
  | IEunop (op, e) ->
     expr destrs e (generate (
     Imunop (op, List.hd destrs, destl)))
  | IEbinop (Msete|Msetne as op, e1, e2) when e1.length > 1 ->
     let tmps1 = multi_fresh_int e1.length in
     let tmps2 = multi_fresh_int e2.length in
     let dst = List.hd destrs in
     let true_l = generate (Ibool (true, dst, destl)) in 
     let false_l = generate (Ibool (false, dst, destl)) in
     let cont, break = if op = Msete then true_l, false_l else false_l, true_l in
     let l = List.fold_right2 (fun r1 r2 cont -> 
                 generate (Imbbranch (Mjne, r2, r1, break, cont))
               ) tmps1 tmps2 cont
     in
     expr tmps1 e1 (expr tmps2 e2 l)
  | IEbinop (op, e1, e2) ->
     let tmp = Register.fresh () in
     expr destrs e1 (
     expr [tmp] e2 (generate (
     Imbinop (op, tmp, List.hd destrs, destl))))
  | IEand (e1, e2) ->
     let true_l = expr destrs e2 destl  in
     let false_l = generate (Ibool (false, List.hd destrs, destl)) in
     condition e1 true_l false_l
  | IEor (e1, e2) ->
     let true_l = generate (Ibool (true, List.hd destrs, destl))  in
     let false_l = expr destrs e2 destl in
     condition e1 true_l false_l

and condition e true_l false_l =
  match e.desc with
  | IEbool b ->
     generate (Igoto (if b then true_l else false_l))
  | IEand (e1, e2) ->
     condition e1 (condition e2 true_l false_l) false_l
  | IEor (e1, e2) ->
     condition e1 true_l (condition e2 true_l false_l)
  | IEunop (Msetei n, e) ->
     let tmp = Register.fresh () in
     expr [tmp] e (generate (
     let op = if n = 0l then Mjz else Mjei n in
     Imubranch (op, tmp, true_l, false_l)))
  | IEunop (Msetnei n, e) -> (* <> n is more likely than = n *)
     let tmp = Register.fresh () in
     expr [tmp] e (generate (
     let op = if n = 0l then Mjz else Mjei n in
     Imubranch (op, tmp, false_l, true_l)))
  | IEunop (Msetgi n | Msetgei n | Msetli n | Msetlei n as op, e) ->
     let tmp = Register.fresh () in
     expr [tmp] e (generate (
     Imubranch (branch_of_unop op, tmp, true_l, false_l)))
  | IEunop (Mnot, e) ->
     condition e false_l true_l
  | IEbinop (Msete|Msetne as op, e1, e2) ->
     let tmps1 = multi_fresh_int e1.length in
     let tmps2 = multi_fresh_int e2.length in
     let cont, break = if op = Msete then  true_l, false_l else false_l, true_l in
     let l = List.fold_right2 (fun r1 r2 cont -> 
                 generate (Imbbranch (Mje, r2, r1, cont, break))
               ) tmps1 tmps2 cont
     in
     expr tmps1 e1 (expr tmps2 e2 l)
  | IEbinop (Msetg | Msetge | Msetl | Msetle as op, e1, e2) ->
     let tmp1 = Register.fresh () in
     let tmp2 = Register.fresh () in
     expr [tmp1] e1 (
     expr [tmp2] e2 (generate (
     Imbbranch (branch_of_binop op, tmp2, tmp1, true_l, false_l))))
  | _ ->
     let tmp = Register.fresh () in
     expr [tmp] e (
         generate (Imubranch (Mjz, tmp, false_l, true_l))
       )

let assign srcrs e destl =
  match e.assignee with
  | Avar v when v = "_" ->
     let dstrs = multi_fresh_int e.length in
     List.fold_right2 (fun src dst l -> generate (Imbinop (Mmov, src, dst, l))) srcrs dstrs destl
  | Avar v ->
     let dstrs = Hashtbl.find locals v in
     List.fold_right2 (fun src dst l -> generate (Imbinop (Mmov, src, dst, l))) srcrs dstrs destl
  | Afield (str, n) ->
     let tmps = multi_fresh_int str.length in
     let dstrs = Utils.sub_list tmps n e.length in
     let l = List.fold_right2 (fun src dst l ->
                 generate (Imbinop (Mmov, src, dst, l))
               ) srcrs dstrs destl
     in
     expr tmps str l
  | Adref (e, n) ->
     let dstr = Register.fresh () in
     let l, _ = List.fold_left (fun (l, n) srcr ->
                 generate (Istore (srcr, dstr, n, l)), n + Utils.word_size
               ) (destl, n) srcrs
     in 
     expr [dstr] e l
     
let rec stmt retrs s exitl destl =
  match s with
  | ISexpr e ->
     begin
       let reduce_munop = function
         | Minc ->
            IDinc
         | Mdec ->
            IDdec
         | _ ->
            assert false
       in
       match e.desc with
       | IEunop (Minc|Mdec as op, { length; desc = IEaccess v }) ->
          let rxs = Hashtbl.find locals v in
          generate (Imunop (op, List.hd rxs, destl))
       (* | IEunop (Minc|Mdec as op, { length; desc = IEselect (str, n) }) ->
        *    let rxs = multi_fresh_int str.length in
        *    expr rxs str (generate (
        *    Iinc_dec_local (reduce_munop op, rxs, n, destl))) *)
       | IEunop (Minc|Mdec as op, { length; desc = IEload (str, n) }) ->
          let tmps = multi_fresh_int str.length in
          expr tmps str (generate (
          Iinc_dec (reduce_munop op, List.hd tmps, n, destl)))
       | _ ->
          assert false
     end
  | IScall (f, actuals) ->
     let _, l_results = List.assoc f !number_formals_results in
     let n_results = Utils.sum_of_list l_results in
     let destrs = multi_fresh_int n_results in
     expr destrs { length = n_results; desc = IEcall (f, actuals) } destl
  | ISprint (fmt, es) -> (* es: list of 8-byte fit expressions *)
     let fmt_reg = Register.fresh () in
     let destrs = multi_fresh_list es in
     let l = generate (Iprint (fmt_reg :: destrs, destl)) in
     let l = generate (Istring (fmt, fmt_reg, l)) in
     List.fold_right2 expr (List.map listify destrs) es l
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
     graph := Label.M.add l (Igoto entry) !graph;
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

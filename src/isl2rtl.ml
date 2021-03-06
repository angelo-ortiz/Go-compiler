
open Rtl

let ptr_fmt = "%p"
let nil_fmt = "<nil>"
let string_fmt = "%s"
            
let graph = ref Label.M.empty
let locals = Hashtbl.create 32
let struct_env = ref Utils.Smap.empty
let number_formals_results = Hashtbl.create 32
let print_functions : (Isl.ident, Rtl.rfundef) Hashtbl.t = Hashtbl.create 32

let listify =
  fun x -> [x]

let reg_of_option = function
  | None -> assert false
  | Some r -> r
         
let multi_fresh_list l =
  List.map (fun _ -> Register.fresh ()) l
  
let multi_fresh_int n =
  let rec loop i acc =
    if i = 0 then acc
    else loop (i-1) (Register.fresh () :: acc)
  in
  loop n []
              
let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l

let branch_of_unop = function
  | Isl.Msetei n -> Rtl.Mjei n
  | Isl.Msetnei n -> Rtl.Mjnei n
  | Isl.Msetgi n -> Rtl.Mjgi n
  | Isl.Msetgei n -> Rtl.Mjgei n
  | Isl.Msetli n -> Rtl.Mjli n
  | Isl.Msetlei n -> Rtl.Mjlei n
  | _ -> assert false

let branch_of_binop = function
  | Isl.Msete -> Rtl.Mje
  | Isl.Msetne -> Rtl.Mjne
  | Isl.Msetg -> Rtl.Mjg
  | Isl.Msetge -> Rtl.Mjge
  | Isl.Msetl -> Rtl.Mjl
  | Isl.Msetle -> Rtl.Mjle
  | _ -> assert false

let rec expr destrs e destl =
  match e.Isl.desc with
  | Isl.IEint n ->
     generate (Iint (n, List.hd destrs, destl))
  | Isl.IEstring s ->
     generate (Istring (s, List.hd destrs, destl))
  | Isl.IEbool b ->
     generate (Ibool (b, List.hd destrs, destl))
  | Isl.IEnil ->
    generate (Iint (0L, List.hd destrs, destl))
  | Isl.IEmalloc n ->
     generate (Imalloc (List.hd destrs, n, destl))
  | Isl.IEaccess v when v = "_" ->
     let rxs = multi_fresh_int e.length in
     List.fold_right2 (fun src dst l ->
         generate (Imbinop (Mmov, src, dst, l))
       ) rxs destrs destl
  | Isl.IEaccess v ->
     let rxs = Hashtbl.find locals v in
     List.fold_right2 (fun src dst l ->
         generate (Imbinop (Mmov, src, dst, l))
       ) rxs destrs destl
  | Isl.IEselect (str, n) ->
     let tmps = multi_fresh_int str.length in
     let srcs = Utils.sub_list tmps n e.length in
     let l =
       List.fold_left2 (fun l src dst ->
           generate (Imbinop (Mmov, src, dst, l))
         ) destl srcs destrs
     in
     expr tmps str l
  | Isl.IEload (str, n) ->
     assert (str.length = 1);
     let srcrs = multi_fresh_int str.length in
     let src = List.hd srcrs in
     let l, _ =
       List.fold_left (fun (l, n) dst ->
           let i = Iload (src, n, dst, l) in
           generate i, n + Utils.word_size
         ) (destl, n) destrs
     in
     expr srcrs str l
  | Isl.IEcall (f, actuals) ->
     let n_formals, n_results = Hashtbl.find number_formals_results f in
     let r_args = List.map multi_fresh_int n_formals in
     let f_args = List.flatten r_args in
     let lab = generate (Icall (destrs, f, f_args, destl)) in
     if List.length actuals < Utils.sum_of_list n_formals
     then expr f_args (List.hd actuals) lab  (* actuals is a single function call *)
     else List.fold_right2 expr r_args actuals lab (* one actual parameter per formal one *)
  | Isl.IEaddr { length; desc = IEaccess v } ->
     let rxs = Hashtbl.find locals v in
     generate (Ilea_local (rxs, 0, List.hd destrs, destl))
  | Isl.IEaddr { length; desc = IEselect (str, n) } ->
     let rxs = multi_fresh_int str.length in
     expr rxs str (generate (
     Ilea_local (rxs, Utils.word_size * n, List.hd destrs, destl)))
  | Isl.IEaddr { length; desc = IEload (str, n) } ->
     let tmps = multi_fresh_int str.length in
     expr tmps str (generate (
     Ilea (List.hd tmps, n, List.hd destrs, destl)))
  | Isl.IEaddr _ -> (* TODO: Really unused??? *)
     assert false
  | Isl.IEunop (op, e) ->
     expr destrs e (generate (
     Imunop (op, List.hd destrs, destl)))
  | Isl.IEbinop (Msete|Msetne as op, e1, e2) when e1.length > 1 ->
     let tmps1 = multi_fresh_int e1.length in
     let tmps2 = multi_fresh_int e2.length in
     let dst = List.hd destrs in
     let true_l = generate (Ibool (true, dst, destl)) in 
     let false_l = generate (Ibool (false, dst, destl)) in
     let cont, break = if op = Msete then true_l, false_l else false_l, true_l in
     let l =
       List.fold_right2 (fun r1 r2 cont -> 
           generate (Imbbranch (Mjne, r2, r1, break, cont))
         ) tmps1 tmps2 cont
     in
     expr tmps1 e1 (expr tmps2 e2 l)
  | Isl.IEbinop (op, e1, e2) ->
     let tmp = Register.fresh () in
     expr destrs e1 (
     expr [tmp] e2 (generate (
     Imbinop (op, tmp, List.hd destrs, destl))))
  | Isl.IEand (e1, e2) ->
     let true_l = expr destrs e2 destl  in
     let false_l = generate (Ibool (false, List.hd destrs, destl)) in
     condition e1 true_l false_l
  | Isl.IEor (e1, e2) ->
     let true_l = generate (Ibool (true, List.hd destrs, destl))  in
     let false_l = expr destrs e2 destl in
     condition e1 true_l false_l
  | Isl.IElist _ -> (* used only in assignments *)
     assert false

and condition e true_l false_l =
  match e.desc with
  | Isl.IEbool b ->
     generate (Igoto (if b then true_l else false_l))
  | Isl.IEand (e1, e2) ->
     condition e1 (condition e2 true_l false_l) false_l
  | Isl.IEor (e1, e2) ->
     condition e1 true_l (condition e2 true_l false_l)
  | Isl.IEunop (Msetei n, e) ->
     let tmp = Register.fresh () in
     expr [tmp] e (generate (
     let op = if n = 0L then Mjz else Mjei n in
     Imubranch (op, tmp, true_l, false_l)))
  | Isl.IEunop (Msetnei n, e) -> (* <> n is more likely than = n *)
     let tmp = Register.fresh () in
     expr [tmp] e (generate (
     let op = if n = 0L then Mjz else Mjei n in
     Imubranch (op, tmp, false_l, true_l)))
  | Isl.IEunop (Msetgi n | Msetgei n | Msetli n | Msetlei n as op, e) ->
     let tmp = Register.fresh () in
     expr [tmp] e (generate (
     Imubranch (branch_of_unop op, tmp, true_l, false_l)))
  | Isl.IEunop (Mnot, e) ->
     condition e false_l true_l
  | Isl.IEbinop (Msete|Msetne as op, e1, e2) ->
     let tmps1 = multi_fresh_int e1.length in
     let tmps2 = multi_fresh_int e2.length in
     let cont, break = if op = Msete then  true_l, false_l else false_l, true_l in
     let l =
       List.fold_right2 (fun r1 r2 cont -> 
           generate (Imbbranch (Mje, r2, r1, cont, break))
         ) tmps1 tmps2 cont
     in
     expr tmps1 e1 (expr tmps2 e2 l)
  | Isl.IEbinop (Msetg | Msetge | Msetl | Msetle as op, e1, e2) ->
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
  match e.Isl.assignee with
  | Isl.Avar v when v = "_" ->
     let dstrs = multi_fresh_int e.length in
     List.fold_right2 (fun src dst l ->
         generate (Imbinop (Mmov, src, dst, l))
       ) srcrs dstrs destl
  | Isl.Avar v ->
     let dstrs = Hashtbl.find locals v in
     List.fold_right2 (fun src dst l ->
         generate (Imbinop (Mmov, src, dst, l))
       ) srcrs dstrs destl
  | Isl.Afield_var (v, n) ->
     let tmps = Hashtbl.find locals v in
     let dstrs = Utils.sub_list tmps n e.length in
     List.fold_right2 (fun src dst l ->
         generate (Imbinop (Mmov, src, dst, l))
       ) srcrs dstrs destl
  | Isl.Afield (str, n) ->
     let tmps = multi_fresh_int str.length in
     let dstrs = Utils.sub_list tmps n e.length in
     let l =
       List.fold_left2 (fun l src dst ->
           generate (Imbinop (Mmov, src, dst, l))
         ) destl srcrs dstrs
     in
     expr tmps str l
  | Isl.Adref (e, n) ->
     let dstr = Register.fresh () in
     let l, _ =
       List.fold_left (fun (l, n) srcr ->
           generate (Istore (srcr, dstr, n, l)), n + Utils.word_size
         ) (destl, n) srcrs
     in 
     expr [dstr] e l

let format_or_type = function
  | Asg.TTint -> Format "%ld"
  | Asg.TTstring -> Format string_fmt
  | Asg.TTnil -> Format nil_fmt
  | Asg.TTunit | Asg.TTuntyped | TTtuple _ -> assert false
  | Asg.TTbool | Asg.TTstruct _ | TTpointer _ as ty -> Type ty
     
let rec translate_print lab ?(seq=true) ?(generate=generate) types_regs =
  let rec rev_ty_reg_list acc blank_control (fmt, regs) = function
    | [], [] ->
       if fmt = "" then acc
       else (Format fmt, List.rev regs) :: acc
    | [], _ | _, [] ->
       assert false
    | ty :: types, in_regs ->
       let rxs, in_regs = Utils.split_list in_regs (Asg2isl.length_of_type ty) in
       match format_or_type ty with
       | Format nxt_fmt ->
          let not_str = nxt_fmt <> string_fmt in
          let add_blank, nxt_blank_control =
            match blank_control with
            | None -> true, None
            | Some prev_not_str -> not_str && prev_not_str, Some not_str
          in
          let fmt =
            if fmt = "" then nxt_fmt
            else fmt ^ (if add_blank then " " else "") ^ nxt_fmt
          in
          let regs =
            if nxt_fmt = "<nil>" then regs
            else List.fold_left (fun xs x -> x :: xs) regs rxs
          in
          rev_ty_reg_list acc nxt_blank_control (fmt, regs) (types, in_regs)
       | Type ty as typ ->
          let add_blank, nxt_blank_control =
            match blank_control with
            | None -> true, None
            | Some prev_not_str -> prev_not_str, Some true
          in
          let acc =
            if fmt = "" then acc
            else
              let fmt = if add_blank then fmt ^ " " else fmt in
              (Format fmt, List.rev regs) :: acc
          in
          let acc = (typ, rxs) :: acc in
          rev_ty_reg_list acc nxt_blank_control ("", []) (types, in_regs)
  in
  let rec loop lab = function
    | [] ->
       lab
    | (Format fmt, regs) :: t_r ->
       let fmt_reg = Register.fresh () in
       let lab = generate (Iprint (fmt_reg :: regs, lab)) in
       let lab = generate (Istring (fmt, fmt_reg, lab)) in
       loop lab t_r
    | (Type TTbool, b) :: t_r ->
       let src = match b with | [r] -> r | _ -> assert false in
       let fmt_reg = Register.fresh () in
       let lab = generate (Iprint ([fmt_reg], lab)) in
       let neqz_l = generate (Istring ("true", fmt_reg, lab)) in
       let eqz_l = generate (Istring ("false", fmt_reg, lab)) in
       loop (generate (Imubranch (Mjz, src, eqz_l, neqz_l))) t_r
    | (Type (TTstruct str), fields) :: t_r ->
       loop (tr_print_struct generate lab str fields) t_r
    | (Type (TTpointer ty), ptr) :: t_r ->
       let src = match ptr with | [r] -> r | _ -> assert false in
       let fmt_reg = Register.fresh () in
       let false_l =
         match ty with
         | TTstruct str ->
            let length = Asg2isl.length_of_type ty in
            let fields = multi_fresh_int length in
            let lab = tr_print_struct generate lab str fields in
            let lab = generate (Iprint ([fmt_reg], lab)) in
            let lab = generate (Istring ("&", fmt_reg, lab)) in
            let lab, _ =
              List.fold_left (fun (l, n) dst ->
                  let i = Iload (src, n, dst, l) in
                  generate i, n + Utils.word_size
                ) (lab, 0) fields
            in lab
         | _ ->
            let l = generate (Iprint (fmt_reg :: ptr, lab)) in
            generate (Istring (ptr_fmt, fmt_reg, l))
       in
       let true_l = generate (Iprint ([fmt_reg], lab)) in
       let true_l = generate (Istring ("<nil>", fmt_reg, true_l)) in
       loop (generate (Imubranch (Mjz, src, true_l, false_l))) t_r
    | (Type _, _) :: _ ->
       assert false
  in
  let blank_control = if seq then Some false else None in
  loop lab (rev_ty_reg_list [] blank_control ("", []) types_regs)

and tr_print_struct generate lab str regs =
  let print_str = ".print_" ^ str in
  if not (Hashtbl.mem print_functions print_str) then begin
      let graph = ref Label.M.empty in
      let fun_ph = {
          formals = multi_fresh_list regs;
          result = [];
          locals = Register.S.empty;
          entry = Label.fresh ();
          exit_ = Label.fresh ();
          body = !graph
        }
      in
      let generate i =
        let l = Label.fresh () in
        graph := Label.M.add l i !graph;
        l
      in
      Hashtbl.add print_functions print_str fun_ph;
      let fmt_end = Register.fresh () in
      let lab = generate (Iprint ([fmt_end], fun_ph.exit_)) in
      let lab = generate (Istring ("}", fmt_end, lab)) in
      let lab =
        translate_print lab ~seq:false ~generate
          (List.map snd (Utils.Smap.find str !struct_env), regs)
      in
      let fmt_begin = Register.fresh () in
      let lab = generate (Iprint ([fmt_begin], lab)) in
      let entry = generate (Istring ("{", fmt_begin, lab)) in
      Hashtbl.replace print_functions print_str { fun_ph with entry; body = !graph }
    end;
  generate (Icall ([], print_str, regs, lab))
     
let rec stmt retrs s exitl destl =
  match s with
  | Isl.ISexpr e ->
     begin
       let reduce_munop = function
         | Isl.Minc -> IDinc
         | Isl.Mdec -> IDdec
         | _ -> assert false
       in
       match e.desc with
       | IEunop (Minc|Mdec as op, { length; desc = IEaccess v }) ->
          let rxs = Hashtbl.find locals v in
          generate (Imunop (op, List.hd rxs, destl))
       (* TODO *)
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
  | Isl.IScall (f, actuals) ->
     let _, l_results = Hashtbl.find number_formals_results f in
     let length = Utils.sum_of_list l_results in
     let destrs = multi_fresh_int length in
     (* the type is unused in `expr` so TTuntyped should suffice *)
     expr destrs { length; desc = IEcall (f, actuals); typ = TTuntyped } destl
  | Isl.ISprint es ->
     let destrs = List.map (fun (e:Isl.iexpr) -> multi_fresh_int e.length) es in
     let types =
       match es with
       | [{ length; desc = IEcall _; typ = TTtuple typ}] -> typ
       | _ -> List.map (fun e -> e.Isl.typ) es
     in
     let l = translate_print destl (types, List.flatten destrs) in
     List.fold_right2 expr destrs es l
  | Isl.ISif (e, bif, belse) ->
     condition e
       (block retrs bif exitl destl)
       (block retrs belse exitl destl)
  | Isl.ISassign (vars, [{ length; desc = IEcall _ } as e]) ->
     let srcrs = List.map (fun (v:Isl.assign) -> multi_fresh_int v.length) vars in
     let l = List.fold_right2 assign srcrs vars destl in
     expr (List.flatten srcrs) e l
  | Isl.ISassign (vars, [{ length; desc = IElist values }]) ->
     assert (List.tl vars = []);
     let srcrs = List.map (fun (v:Isl.iexpr) -> multi_fresh_int v.length) values in
     let l = assign (List.flatten srcrs) (List.hd vars) destl in
     List.fold_right2 expr srcrs values l
  | Isl.ISassign (vars, values) ->
     let srcrs = List.map (fun (v:Isl.iexpr) -> multi_fresh_int v.length) values in
     let l = List.fold_right2 assign srcrs vars destl in
     List.fold_right2 expr srcrs values l
  | Isl.ISreturn [{ length; desc = IEcall (f, actuals) } as e] ->
     expr (List.flatten retrs) e exitl
  | Isl.ISreturn es ->
     List.fold_right2 expr retrs es exitl
  | Isl.ISfor (e, bfor) ->
     let l = Label.fresh () in
     let entry = condition e (block retrs bfor exitl l) destl in
     graph := Label.M.add l (Igoto entry) !graph;
     entry

and block retrs b exitl destl =
  List.fold_right (fun st dlab -> stmt retrs st exitl dlab) b destl
  
let funct (f:Isl.ifundef) =
  let r_formals = List.map (fun (_, n) -> multi_fresh_int n) f.formals in
  List.iter2 (fun (v, _) rs -> Hashtbl.add locals v rs) f.formals r_formals;
  let result = List.map (fun len -> multi_fresh_int len) f.result in
  let local_vars =
    List.fold_left (fun set (v, n) ->
        let regs = multi_fresh_int n in
        Hashtbl.add locals v regs;
        List.fold_left (fun set r -> Register.S.add r set) set regs
      ) Register.S.empty f.locals
  in
  let exit_ = Label.fresh () in
  let entry = block result f.body exit_ exit_ in
  let body = !graph in
  Hashtbl.reset locals;
  graph := Label.M.empty;
  {
    formals = List.flatten r_formals;
    result = List.flatten result;
    locals = local_vars;
    entry; exit_; body
  } 
  
let programme (p:Isl.iprogramme) =
  let add_retrs f (def:Isl.ifundef) =
    Hashtbl.add number_formals_results f (List.map snd def.formals, def.result)
  in
  struct_env := p.structs;
  Utils.Smap.iter add_retrs p.functions;
  let functions = Utils.Smap.map funct p.functions in
  {
    structs = p.structs;
    functions = Hashtbl.fold Utils.Smap.add print_functions functions
  }

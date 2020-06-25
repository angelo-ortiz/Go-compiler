
open Ertl

type info = {
    instr : Ertl.einstr;
    def : Register.set;
    use : Register.set;
    succ : Label.set;
    pred : Label.set;
    in_ : Register.set;
    out_ : Register.set;
  }

let perform_analysis graph =
  
  let def_use = function
    | Iint (_, r, _) | Istring (_, r, _) | Ibool (_, r, _)
    | Iget_param (_, r, _) | Ipop_param (r, _) ->
       Register.S.singleton r, Register.S.empty
    | Ilea_local (src, _, dst, _) | Ilea (src, _, dst, _) | Iload (src, _, dst, _)
    | Imbinop (Mmov, src, dst, _) ->
       Register.S.singleton dst, Register.S.singleton src
    | Iidiv_imm (_, _) ->
       let set = Register.S.add Register.rax (Register.S.singleton Register.rdx) in
       set, set
    | Imunop (_, r, _) ->
       let set = Register.S.singleton r in
       set, set
    | Iidiv (src,  _) ->
       let def = Register.S.add Register.rax (Register.S.singleton Register.rdx) in
       def, Register.S.add src def
    | Imbinop (_, src, dst, _) ->
       let def = Register.S.singleton dst in
       def, Register.S.add src def
    | Imubranch (_, r, _, _) | Ipush_param (r, _) | Iset_result (r, _, _)
    | Iinc_dec (_, r, _, _) ->
       Register.S.empty, Register.S.singleton r
    | Istore (r1, r2, _, _) | Imbbranch (_, r2, r1, _, _) ->
       Register.S.empty, Register.S.add r1 (Register.S.singleton r2)
    | Igoto _ | Ialloc_frame _ | Ifree_frame _ | Ialloc_stack _ | Ifree_stack _ ->
       Register.S.empty, Register.S.empty
    | Icall (_, n_r_args, _) ->
       Register.S.of_list Register.caller_saved,
       Register.S.of_list (fst (Utils.split_list Register.parameters n_r_args))
    | Ireturn ->
       Register.S.empty, Register.S.add Register.rax (Register.S.of_list Register.callee_saved)
  in
  
  let succ = function
    | Iint (_, _, l) | Istring (_, _, l) | Ibool (_, _, l) | Ilea_local (_, _, _, l)
    | Ilea (_, _, _, l) | Iload (_, _, _, l) | Istore (_, _, _, l) | Icall (_, _, l)
    | Imunop (_, _, l) | Iidiv_imm (_, l) | Iidiv (_, l) | Iinc_dec (_, _, _, l)
    | Imbinop (_, _, _, l) | Igoto l | Ialloc_frame l | Ifree_frame l | Ialloc_stack (_, l)
    | Ifree_stack (_, l) | Iget_param (_, _, l) | Ipop_param (_, l) | Ipush_param (_, l)
    | Iset_result (_, _, l) ->
       Label.S.singleton l
    | Imubranch (_, _, l1, l2) | Imbbranch (_, _, _, l1, l2) ->
       Label.S.add l1 (Label.S.singleton l2)
    | Ireturn ->
       Label.S.empty
  in
  
  let in_vars def use out_ =
    Register.S.union use (Register.S.diff out_ def)  
  in
  
  let out_vars info_map succ =
    Label.S.fold (
        fun s set ->
        let info = Label.M.find s info_map in
        Register.S.union set info.in_
      ) succ Register.S.empty  
  in
  
  let info_of_instr instr =
    let def, use = def_use instr in
    let succ = succ instr in
    let pred = Label.S.empty in
    let out_ = Register.S.empty in
    let in_ = in_vars def use out_ in
    { instr; def; use; succ; pred; in_; out_ }
  in
  
  let set_preds info_map =
    Label.M.fold (
        fun l _ map ->
        Label.S.fold (
            fun n_l map' ->
            let n_info = Label.M.find n_l map' in
            Label.M.add n_l { n_info with pred = Label.S.add l n_info.pred } map'
          ) (Label.M.find l map).succ map
      ) info_map info_map  
  in
  
  let initialise graph =  
    let info_map, set =
      Label.M.fold (
          fun l i (map, set) ->
          Label.M.add l (info_of_instr i) map, Label.S.add l set
        ) graph (Label.M.empty, Label.S.empty)
    in
    set_preds info_map, set
  in
  
  let rec kildall (info_map, working_set) =
    if Label.S.is_empty working_set then info_map
    else begin
        let l = Label.S.choose working_set in
        let ws = Label.S.remove l working_set in
        let info = Label.M.find l info_map in
        let old_in_ = info.in_ in
        let out_ = out_vars info_map info.succ in
        let in_ = in_vars info.def info.use out_ in
        let info_map = Label.M.add l { info with in_; out_ } info_map in
        let ws = if Register.S.subset in_ old_in_ then ws else Label.S.union ws info.pred in
        kildall (info_map, ws)
      end
  in

  kildall (initialise graph)


open Ertltree

type info = {
    instr : Ertltree.instr;
    def : Register.set;
    use : Register.set;
    succ : Label.set;
    pred : Label.set;
    in_ : Register.set;
    out_ : Register.set;
  }

let kildall g =
  let def_use = function
    | Eint (_, r, _) | Estring (_, r, _) | Ebool (_, r, _) | Eget_param (_, r, _) ->
       Register.S.singleton r, Register.S.empty
    | Elea (src, dst, _) | Eload (src, _, dst, _) ->
       Register.S.singleton dst, Register.S.singleton src
    | Emunop (Istree.Midivil _, r, _) ->
       assert (r = Register.rax);
       let set = Register.S.add r (Register.S.singleton Register.rdx) in
       set, set
    | Emunop (_, r, _) ->
       Register.S.singleton r, Register.S.singleton r
    | Embinop (Midiv, src, dst, _) ->
       assert (dst = Register.rax);
       let def = Register.S.add dst (Register.S.singleton Register.rdx) in
       def, Register.S.add src def
    | Embinop (_, src, dst, _) ->
       let def = Register.S.singleton dst in
       def, Register.S.add src def
    | Emubranch (_, r, _, _) | Epush_param (r, _) ->
       Register.S.empty, Register.S.singleton r
    | Estore_field (r1, r2, _, _) | Estore_dref (r1, r2, _) | Embbranch (_, r2, r1, _, _) ->
       Register.S.empty, Register.S.add r1 (Register.S.singleton r2)
    | Egoto _ | Ealloc_frame _ | Efree_frame _ ->
       Register.S.empty, Register.S.empty
    | Ecall (n_res, _, n_r_args, _) ->
       Register.S.of_list Register.caller_saved,
       Register.S.of_list (Utils.prefix n_r_args Register.parameters)
    | Ereturn -> (* TODO: what about the other return registers??? *)
       Register.S.empty, Register.S.add Register.rax (Register.S.of_list Register.callee_saved)
  in
  
  let succ = function
    | Eint (_, _, l) | Estring (_, _, l) | Ebool (_, _, l) | Elea (_, _, l)
    | Eload (_, _, _, l) | Estore_field (_, _, _, l) | Estore_dref (_, _, l)
    | Ecall (_, _, _, l) | Emunop (_, _, l) | Embinop (_, _, _, l) | Egoto l
    | Ealloc_frame l | Efree_frame l | Eget_param (_, _, l) | Epush_param (_, l) ->
       Label.S.singleton l
    | Emubranch (_, _, l1, l2) | Embbranch (_, _, _, l1, l2) ->
       Label.S.add l1 (Label.S.singleton l2)
    | Ereturn ->
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
  
  let initialise g =  
    let info_map, set =
      Label.M.fold (
          fun l i (map, set) ->
          Label.M.add l (info_of_instr i) map, Label.S.add l set
        ) g (Label.M.empty, Label.S.empty)
    in
    set_preds info_map, set
  in
  
  let rec loop (info_map, ws) =
    if Label.S.is_empty ws then info_map
    else begin
        let l = Label.S.choose ws in
        let ws = Label.S.remove l ws in
        let info = Label.M.find l info_map in
        let old_in_ = info.in_ in
        let out_ = out_vars info_map info.succ in
        let in_ = in_vars info.def info.use out_ in
        let info_map = Label.M.add l { info with in_; out_ } info_map in
        let ws = if Register.S.subset in_ old_in_ then ws else Label.S.union ws info.pred in
        loop (info_map, ws)
      end
  in

  loop (initialise g)

    

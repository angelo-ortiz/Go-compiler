
open Ertltree

type features = {
    def : Register.set;
    use : Register.set;
    succ : Label.set;
    mutable pred : Label.set;
    mutable in_ : Register.set;
    mutable out_ : Register.set;
  }

let instr_table : (Label.t, features) Hashtbl.t = Hashtbl.create 64

(* TODO *)
let def_use = function
  | Eint _ ->
     assert false
  | _ ->
     assert false

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

let in_vars def use out_ =
  Register.S.union use (Register.S.diff out_ def)

let out_vars succ =
  Label.S.fold (
      fun s set ->
      let feats = Hashtbl.find instr_table s in
      Register.S.union set feats.in_
    ) succ Register.S.empty
  
let feats_of_instr instr =
  let def, use = def_use instr in
  let succ = succ instr in
  let pred = Label.S.empty in
  let out_ = Register.S.empty in
  let in_ = in_vars def use out_ in
  { def; use; succ; pred; in_; out_ }

let set_preds () =
  Hashtbl.iter (
      fun l fs ->
      Label.S.iter (
          fun n_l ->
          let n_fs = Hashtbl.find instr_table n_l in
          n_fs.pred <- Label.S.add n_l n_fs.pred 
        ) fs.succ
    ) instr_table
  
let init_table g =  
  let set =
    Label.M.fold (
        fun l i set ->
        let set = Label.S.add l set in
        Hashtbl.add instr_table l (feats_of_instr i);
        set
      ) g Label.S.empty
  in
  set_preds ();
  set

let kildall g =
  let initial_ws = init_table g in
  let rec loop ws =
    if not (Label.S.is_empty ws) then begin
        let l = Label.S.choose ws in
        let ws = Label.S.remove l ws in
        let feats = Hashtbl.find instr_table l in
        let old_in_ = feats.in_ in
        feats.out_ <- out_vars feats.succ;
        feats.in_ <- in_vars feats.def feats.use feats.out_;
        if not (Register.S.subset feats.in_ old_in_) then
          loop (Label.S.union ws feats.pred)
      end
  in
  loop initial_ws

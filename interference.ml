
open Ertltree
open Liveness
   
type arcs = {
    prefs : Register.set;
    intfs : Register.set;
  }

type graph = arcs Register.map

let no_arcs = {
    prefs = Register.S.empty;
    intfs = Register.S.empty;
  }

(* Arc = directed edge: tail (tl) -> head (hd) *)

let degree_of v g =
  Register.S.cardinal (Register.M.find v g).intfs

let remove_pref_arc hd tl g =
  let neighs = Register.M.find tl g in
  Register.M.add tl { neighs with prefs = Register.S.remove hd neighs.prefs } g 

let remove_pref_edge v w g =
  let g = remove_pref_arc v w g in
  remove_pref_arc w v g
  
let replace_pref_arc p_hd n_hd tl g =
  let neighs = Register.M.find tl g in
  let prefs = Register.S.remove p_hd neighs.prefs in
  let prefs = Register.S.add n_hd prefs in
  Register.M.add tl { neighs with prefs } g 

let add_pref_arc hd tl g =
  let neighs = Register.M.find tl g in
  Register.M.add tl { neighs with prefs = Register.S.add hd neighs.prefs } g 

let remove_intf_arc hd tl g =
  let neighs = Register.M.find tl g in
  Register.M.add tl { neighs with intfs = Register.S.remove hd neighs.intfs } g 

let remove_intf_edge v w g =
  let g = remove_intf_arc v w g in
  remove_intf_arc w v g
  
let replace_intf_arc p_hd n_hd tl g =
  let neighs = Register.M.find tl g in
  let intfs = Register.S.remove p_hd neighs.intfs in
  let intfs = Register.S.add n_hd intfs in
  Register.M.add tl { neighs with intfs } g 

let add_intf_arc hd tl g =
  let neighs = Register.M.find tl g in
  Register.M.add tl { neighs with intfs = Register.S.add hd neighs.intfs } g 

let remove_node v arcs g =
  let g = (* remove preference arcs from its neighbours to [v] *)
    Register.S.fold (remove_pref_arc v) arcs.prefs g in
  let g = (* idem for interference arcs *)
    Register.S.fold (remove_intf_arc v) arcs.intfs g
  in
  Register.M.remove v g

(** [merge_nodes v1 v2 g] merges v1 into v2, which must be linked by a preference edge **)
let merge_nodes v1 v2 g =
  let g = remove_pref_arc v1 v2 g in
  let v2_arcs = Register.M.find v2 g in
  let v1_arcs = Register.M.find v1 g in
  let v1_arcs = { v1_arcs with prefs = Register.S.remove v2 v1_arcs.prefs } in
  let g =
    Register.S.fold (
        fun w g -> (* add only non-conflicting preference arcs *)
        if Register.S.mem w v2_arcs.intfs then remove_pref_arc v1 w g
        else
          let g = replace_pref_arc v1 v2 w g in
          add_pref_arc w v2 g
      ) v1_arcs.prefs g
  in
  let g =
    Register.S.fold (
        fun w g ->
        let g = if Register.S.mem w v2_arcs.prefs then remove_pref_edge v2 w g else g in
        let g = replace_intf_arc v1 v2 w g in
        add_intf_arc w v2 g
      ) v1_arcs.intfs g
  in
  Register.M.remove v1 g

let build_graph info_map =

  let update_prefs tl arcs hd g =
    Register.M.add tl { arcs with prefs = Register.S.add hd arcs.prefs } g
  in
    
  let add_pref_edge v w g =
    let add_one_arc tl hd g =
      try
        let arcs = Register.M.find tl g in
        if Register.S.mem hd arcs.intfs then g
        else update_prefs tl arcs hd g
      with Not_found ->
        update_prefs tl no_arcs hd g
    in
    add_one_arc w v (add_one_arc v w g)
  in

  let add_intf_edge v w g =
    let add_one_arc tl hd g =
      try
        let arcs = Register.M.find tl g in
        let prefs = Register.S.remove hd arcs.prefs in
        let intfs = Register.S.add hd arcs.intfs in
        Register.M.add tl { prefs; intfs} g
      with Not_found ->
        Register.M.add tl { no_arcs with intfs = Register.S.singleton hd } g
    in
    add_one_arc w v (add_one_arc v w g)
  in
  
  let update _ info g =
    match info.instr with
    | Embinop (Istree.Mmov, w, v, _) ->
       let g = add_pref_edge v w g in
       let out_live = Register.S.remove v info.out_ in
       Register.S.fold (add_intf_edge v) (Register.S.remove w out_live) g
    | _ ->
       Register.S.fold (
           fun v g ->
           Register.S.fold (add_intf_edge v) (Register.S.remove v info.out_) g
         ) info.def g
  in
  
  Label.M.fold update info_map Register.M.empty

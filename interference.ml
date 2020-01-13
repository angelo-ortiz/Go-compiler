
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

let degree_of v graph =
  Register.S.cardinal (Register.M.find v graph).intfs

let remove_pref_arc hd tl graph =
  let neighs = Register.M.find tl graph in
  Register.M.add tl { neighs with prefs = Register.S.remove hd neighs.prefs } graph

let remove_pref_edge v w graph =
  let graph = remove_pref_arc v w graph in
  remove_pref_arc w v graph
  
let replace_pref_arc p_hd n_hd tl graph =
  let neighs = Register.M.find tl graph in
  let prefs = Register.S.remove p_hd neighs.prefs in
  let prefs = Register.S.add n_hd prefs in
  Register.M.add tl { neighs with prefs } graph 

let add_pref_arc hd tl graph =
  let neighs = Register.M.find tl graph in
  Register.M.add tl { neighs with prefs = Register.S.add hd neighs.prefs } graph

let remove_intf_arc hd tl graph =
  let neighs = Register.M.find tl graph in
  Register.M.add tl { neighs with intfs = Register.S.remove hd neighs.intfs } graph

let remove_intf_edge v w g =
  let g = remove_intf_arc v w g in
  remove_intf_arc w v g
  
let replace_intf_arc p_hd n_hd tl graph =
  let neighs = Register.M.find tl graph in
  let intfs = Register.S.remove p_hd neighs.intfs in
  let intfs = Register.S.add n_hd intfs in
  Register.M.add tl { neighs with intfs } graph 

let add_intf_arc hd tl graph =
  let neighs = Register.M.find tl graph in
  Register.M.add tl { neighs with intfs = Register.S.add hd neighs.intfs } graph 

let remove_node v arcs graph =
  let graph = (* remove preference arcs from its neighbours to [v] *)
    Register.S.fold (remove_pref_arc v) arcs.prefs graph in
  let graph = (* idem for interference arcs *)
    Register.S.fold (remove_intf_arc v) arcs.intfs graph
  in
  Register.M.remove v graph

(** [merge_nodes v1 v2 g] merges v1 into v2, which must be linked by a preference edge **)
let merge_nodes v1 v2 graph =
  let graph = remove_pref_arc v1 v2 graph in
  let v2_arcs = Register.M.find v2 graph in
  let v1_arcs = Register.M.find v1 graph in
  let v1_arcs = { v1_arcs with prefs = Register.S.remove v2 v1_arcs.prefs } in
  let graph =
    Register.S.fold (
        fun w g -> (* add only non-conflicting preference arcs *)
        if Register.S.mem w v2_arcs.intfs then remove_pref_arc v1 w g
        else
          let g = replace_pref_arc v1 v2 w g in
          add_pref_arc w v2 g
      ) v1_arcs.prefs graph
  in
  let graph =
    Register.S.fold (
        fun w g ->
        let g = if Register.S.mem w v2_arcs.prefs then remove_pref_edge v2 w g else g in
        let g = replace_intf_arc v1 v2 w g in
        add_intf_arc w v2 g
      ) v1_arcs.intfs graph
  in
  Register.M.remove v1 graph

let build_graph info_map =

  let update_prefs tl arcs hd graph =
    Register.M.add tl { arcs with prefs = Register.S.add hd arcs.prefs } graph
  in
    
  let add_pref_edge v w graph =
    let add_one_arc tl hd g =
      try
        let arcs = Register.M.find tl g in
        if Register.S.mem hd arcs.intfs then g
        else update_prefs tl arcs hd g
      with Not_found ->
        update_prefs tl no_arcs hd g
    in
    add_one_arc w v (add_one_arc v w graph)
  in

  let add_intf_edge v w graph =
    let add_one_arc tl hd g =
      try
        let arcs = Register.M.find tl g in
        let prefs = Register.S.remove hd arcs.prefs in
        let intfs = Register.S.add hd arcs.intfs in
        Register.M.add tl { prefs; intfs} g
      with Not_found ->
        Register.M.add tl { no_arcs with intfs = Register.S.singleton hd } g
    in
    add_one_arc w v (add_one_arc v w graph)
  in
  
  let update _ info graph =
    match info.instr with
    | Imbinop (Mmov, w, v, _) ->
       let graph = add_pref_edge v w graph in
       let out_live = Register.S.remove v info.out_ in
       Register.S.fold (add_intf_edge v) (Register.S.remove w out_live) graph
    | _ ->
       Register.S.fold (
           fun v g ->
           Register.S.fold (add_intf_edge v) (Register.S.remove v info.out_) g
         ) info.def graph
  in
  
  Label.M.fold update info_map Register.M.empty

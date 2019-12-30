
open Interference

type colour =
  | Spilled of int
  | Reg of Register.t

type colouring = colour Register.map

module IntSet = Set.Make( struct let compare = Pervasives.compare type t = int end )

exception Bad_pref_arc
exception Good_pref_arc of Register.t * Register.t
exception Found_node of Register.t * Interference.arcs
exception Found_colour of int
        
let alloc_registers k mach_regs g =

  let stack = Stack.create () in
  let spilled_set = ref Register.S.empty
  in

  let usage_counter =
    let incr_counter v map =
      let c =
        try Register.M.find v map
        with Not_found -> 0
      in
      Register.M.add v (c+1) map
    in
    Register.M.fold (
        fun v arcs u_c ->
        let u_c = Register.S.fold incr_counter arcs.prefs u_c in
        Register.S.fold incr_counter arcs.intfs u_c
      ) g Register.M.empty
  in

  let init_degree =
    Register.M.map (
        fun arcs -> Register.S.cardinal arcs.intfs
      ) g
  in

  let next_spilled =
    let counter = ref 0 in
    fun () -> incr counter; !counter
  in

  let is_machine_register r =
    Register.S.mem r mach_regs
  in
  
  let min_cost_node g =
    let min_node =
      Register.M.fold (
          fun v _ min_n ->
          if Register.S.mem v mach_regs then min_n
          else begin
              let cost =
                float_of_int (Register.M.find v usage_counter)
                /. (float_of_int (Register.M.find v init_degree))
              in
              match min_n with
              | None ->
                 Some (v, cost)
              | Some (s, c) when cost < c ->
                 Some (v, cost)
              | m_n ->
                 m_n
            end
        ) g None
    in match min_node with
       | Some (v, _) ->
          v
       | None ->
          assert false 
  in

  (* TODO: what about checking colourability based on the initial graph g???
     => no need to store the "current" graph, but finding the set of possible colours 
     would be longer *)
  let colour_node c g v =
    let _, arcs = Stack.pop stack in
    let poss_colours =
      Register.S.fold (
          fun w colours ->
          match Register.M.find w c with
          | Spilled _ -> colours
          | Reg r -> Register.S.remove r colours
        ) arcs.intfs mach_regs
    in
    let v_c = Register.S.choose poss_colours in
    Register.M.add v (Reg v_c) c
  in

  let george_criterion g =
    let check_couple v2 v2_neighs v2_is_mach_reg v1 =
      let v1_neighs = (Register.M.find v2 g).intfs in
      Register.S.iter (
          fun w ->
          if (is_machine_register w <> v2_is_mach_reg || Interference.degree_of w g >= k)
             && not (Register.S.mem w v2_neighs)
          then raise Bad_pref_arc
        ) v1_neighs
    in
    let check_node v2 arcs =
      let is_mach_reg = is_machine_register v2 in
      let neighs = arcs.prefs in
      Register.S.iter (
          fun v1 ->
          try check_couple v2 neighs is_mach_reg v1; raise (Good_pref_arc (v1, v2))
          with Bad_pref_arc -> ()
        ) neighs
    in
    Register.M.iter check_node g
  in

  (* TODO: derecursify these mutually recursive functions
     big while loop + stack + sum type (Sim/C/F/S/Sel) maybe Col too??? *)
  let rec simplify c g =
    let v_opt =
      Register.M.fold (
          fun v arcs min_n ->
          if Register.S.mem v mach_regs || not (Register.S.is_empty arcs.prefs) then min_n
          else begin
              let deg = Register.S.cardinal arcs.intfs in
              if deg >= k then min_n
              else begin
                  match min_n with
                  | None ->
                     Some (v, deg)
                  | Some (s, d) when deg < d ->
                     Some (v, deg)
                  | m_n ->
                     m_n
                end
            end
        ) g None
    in
    match v_opt with
    | None ->
       coalesce c g
    | Some (v, _) ->
       select c g v
      
  and coalesce c g =
    try
      george_criterion g;
      freeze c g
    with Good_pref_arc (v1, v2) ->
      let g = Interference.merge_nodes v1 v2 g in
      let c = simplify c g in
      Register.M.add v1 (Register.M.find v2 c) c
    
  and freeze c g =
    try
      Register.M.iter (
          fun v arcs ->
          if Interference.degree_of v g < k then raise (Found_node (v, arcs))
        ) g;
      spill c g
    with Found_node (v, arcs) ->
      let g =
        Register.S.fold (
            fun w g ->
            let arcs' = Register.M.find w g in
            Register.M.add w { arcs' with prefs = Register.S.remove v arcs'.prefs } g
          ) arcs.prefs g
      in
      let g = Register.M.add v { arcs with prefs = Register.S.empty } g in
      simplify c g
      
      
  and spill c g =
    if Register.M.is_empty g then c
    else
      let v = min_cost_node g in
      select c g v
      
  and select c g v =
    let arcs = Register.M.find v g in
    Stack.push (v, arcs) stack;
    let g = Interference.remove_node v arcs g in
    (* Format.printf "Node removed == %a\n" Register.string_of_reg v; *)
    (* Format.printf "%a\n----------\n" Pretty_printer.pp_g g; *)
    let c = simplify c g in
    try colour_node c g v
    with Not_found ->
      spilled_set := Register.S.add v !spilled_set;
      Register.M.add v (Spilled (next_spilled ())) c
  in
  
  let init_colouring =
    Register.S.fold (
        fun r c -> Register.M.add r (Reg r) c
      ) mach_regs Register.M.empty
  in

  let reduce_spilled spilled =
    let reduce_arcs arcs =
      let intfs = Register.S.inter arcs.intfs spilled in
      let prefs = Register.S.inter arcs.prefs spilled in
      { intfs; prefs }, not (Register.S.is_empty prefs)
    in
    let rec coalesce g rem =
      if Register.S.is_empty rem then g
      else begin
          let v = Register.S.choose rem in
          let prefs = (Register.M.find v g).prefs in
          if Register.S.is_empty prefs then coalesce g (Register.S.remove v rem)
          else begin
              let g, rem =
                Register.S.fold (
                    fun w (g, rem) ->
                    Interference.merge_nodes w v g, Register.S.remove w rem
                  ) prefs (g, rem)
              in
              coalesce g rem
            end
        end
    in
    let min_degree_node g =
      Register.M.fold (
          fun v _ m_n ->
          let deg = Interference.degree_of v g in
          match m_n with
          | None ->
             Some (v, deg)
          | Some (s, d) when deg < d ->
             Some (v, deg)
          | m_s ->
             m_s
        ) g None
    in
    let colour_node sc g v =
      let neighs = (Register.M.find v g).intfs in
      let min_possible_colour constraints =
        IntSet.fold (
            fun c n ->
            if n < c then raise (Found_colour n) else n + 1
          ) constraints 1
      in
      let taken_colours =
        Register.S.fold (
            fun w colours ->
            try
              let c = Register.M.find w sc in
              IntSet.add c colours
            with Not_found -> colours
          ) neighs IntSet.empty
      in
      let v_c = min_possible_colour taken_colours in
      Register.M.add v v_c sc
    in
    let simplify g =
      let curr_g = ref g in
      let to_colour = Stack.create () in
      while not (Register.M.is_empty !curr_g) do
        let min_node = match min_degree_node !curr_g with | Some (v, _) -> v | None -> assert false in
        curr_g := Interference.remove_node min_node (Register.M.find min_node !curr_g) !curr_g;
        Stack.push min_node to_colour
      done;
      let sc = ref Register.M.empty in
      while not (Stack.is_empty to_colour) do
        let v = Stack.pop to_colour in
        sc := colour_node !sc g v;
      done;
      !sc
    in
    let g, have_prefs =
      Register.S.fold (
          fun r (g', hp) ->
          let arcs', has_prefs = reduce_arcs (Register.M.find r g) in
          Register.M.add r arcs' g', if has_prefs then Register.S.add r hp else hp
        ) spilled (Register.M.empty, Register.S.empty)
    in
    let combine_colourings =
      Register.M.fold (
          fun sp n col ->
          Register.M.add sp (Spilled n) col
        )
    in
    let g = coalesce g have_prefs in
    (* Format.printf "spilled == %a\n" Pretty_printer.pp_set spilled;
     * Format.printf "%a\n----------\n" Pretty_printer.pp_g g; *)
    combine_colourings (simplify g)
    (* combine_colourings (simplify (coalesce g have_prefs)) *)

  in
  reduce_spilled !spilled_set (simplify init_colouring g)

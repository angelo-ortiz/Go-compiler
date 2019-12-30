
open Interference

type colour =
  | Spilled of int
  | Reg of Register.t

type colouring = colour Register.map

module IntSet = Set.Make( struct let compare = Pervasives.compare type t = int end )

type action =
  | Simplify
  | Select of Register.t
  | Coalesce
  | Freeze
  | Spill
  | Colour_node of Register.t * Interference.arcs
  | Copy_colour of Register.t * Register.t
              
exception Bad_pref_arc
exception Good_pref_arc of Register.t * Register.t
exception Found_node of Register.t * Interference.arcs
exception Found_colour of int
exception No_possible_colour
        
let alloc_registers mach_regs g =

  let k = Register.S.cardinal mach_regs
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

  let initial_degrees =
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
                /. (float_of_int (Register.M.find v initial_degrees))
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
  let colour_node c v arcs =
    let poss_colours =
      Register.S.fold (
          fun w colours ->
          match Register.M.find w c with
          | Spilled _ ->
             colours
          | Reg r ->
             Register.S.remove r colours
        ) arcs.intfs mach_regs
    in
    try 
      let v_c = Register.S.choose poss_colours in
      Register.M.add v (Reg v_c) c
    with Not_found ->
      raise No_possible_colour
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

  let min_pref_disconnected_node g =
    Register.M.fold (
        fun v arcs min_n ->
        if not (Register.S.is_empty arcs.prefs) then min_n
        else begin
            let deg = Register.S.cardinal arcs.intfs in
            if deg >= k then min_n
            else match min_n with
                 | None ->
                    Some (v, deg)
                 | Some (s, d) when deg < d ->
                    Some (v, deg)
                 | m_n ->
                    m_n
          end
      ) g None
  in

  let george_appel c g =
    let spilled = ref Register.S.empty in
    let curr_g = ref g in
    let curr_c = ref c in
    let stack = Stack.create () in
    Stack.push Simplify stack;
    while not (Stack.is_empty stack) do
      match Stack.pop stack with
      | Simplify ->
         begin
           (* Format.printf "Action #1\n"; *)
           match min_pref_disconnected_node !curr_g with
           | None ->
              Stack.push Coalesce stack
           | Some (v, _) ->
              Stack.push (Select v) stack
         end
      | Select v ->
         begin
           (* Format.printf "Action #2\n";
            * Format.printf "Register deleted %a\n" Register.string_of_reg v; *)
           let arcs = Register.M.find v !curr_g in
           Stack.push (Colour_node (v, arcs)) stack;
           curr_g := Interference.remove_node v arcs !curr_g;
           Stack.push Simplify stack
         end
      | Coalesce ->
         begin
           (* Format.printf "Action #3\n"; *)
           try
             george_criterion !curr_g;
             Stack.push Freeze stack
           with Good_pref_arc (v1, v2) ->
             curr_g := Interference.merge_nodes v1 v2 !curr_g;
             Stack.push (Copy_colour (v2, v1)) stack;
             Stack.push Simplify stack
         end
      | Freeze ->
         begin
           (* Format.printf "Action #4\n"; *)
           try
             Register.M.iter (
                 fun v arcs ->
                 if Register.S.cardinal arcs.intfs < k then raise (Found_node (v, arcs))
               ) !curr_g;
             Stack.push Spill stack
           with Found_node (v, arcs) ->
             (* Format.printf "Register frozen %a\n" Register.string_of_reg v; *)
             curr_g := Register.S.fold (Interference.remove_pref_arc v) arcs.prefs !curr_g;
             curr_g := Register.M.add v { arcs with prefs = Register.S.empty } !curr_g;
             Stack.push Simplify stack
         end
      | Spill ->
         begin
           (* Format.printf "Action #5\n"; *)
           if not (Register.M.is_empty !curr_g) then 
             let v = min_cost_node !curr_g in
             Stack.push (Select v) stack
         end
      | Colour_node (v, arcs) ->
         begin
           (* Format.printf "Action #6\n"; *)
           try
             if not (is_machine_register v) then curr_c := colour_node !curr_c v arcs
           with No_possible_colour ->
             spilled := Register.S.add v !spilled;
             curr_c := Register.M.add v (Spilled (next_spilled ())) !curr_c
         end
      | Copy_colour (col, uncol) ->
         (* Format.printf "Action #7\n"; *)
         curr_c := Register.M.add uncol (Register.M.find col !curr_c) !curr_c
    done;
    !spilled, !curr_c
  in
  
  let reduce_spilled (spilled, colouring) =
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
    combine_colourings (simplify (coalesce g have_prefs)) colouring
  in
  
  let initial_colouring =
    Register.S.fold (
        fun r c -> Register.M.add r (Reg r) c
      ) mach_regs Register.M.empty
  in
  
  reduce_spilled (george_appel initial_colouring g)

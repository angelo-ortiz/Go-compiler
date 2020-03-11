
open Interference

type colour =
  | Reg of Register.t
  | Spilt of int
  | Heap of int * int (* stack location, heap location *)

type colouring = colour Register.map

module IntSet = Set.Make( struct let compare = compare type t = int end )

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
exception Found_colour of colour
exception Found_spilt of int
exception No_possible_colour

let print_colour fmt = function
  | Reg mr ->
     Format.fprintf fmt "reg %a" Register.string_of_reg mr
  | Spilt n ->
     Format.fprintf fmt "spilt %d" n
  | Heap (s, h) ->
     Format.fprintf fmt "heap s:%d, h:%d" s h
               

let reduce_spilt spilt colouring graph heap_map =
  
  let copy_colour = Stack.create () in
  let n_locals = ref 0
  in

  let reduce_arcs arcs =
    let intfs = Register.S.inter arcs.intfs spilt in
    let prefs = Register.S.inter arcs.prefs spilt in
    { intfs; prefs }, not (Register.S.is_empty prefs)
  in

  let rec coalesce graph rem =
    if Register.S.is_empty rem then graph
    else begin
        let v = Register.S.choose rem in
        let prefs = (Register.M.find v graph).prefs in
        if Register.S.is_empty prefs then coalesce graph (Register.S.remove v rem)
        else begin
            let graph, rem =
              Register.S.fold (fun w (g, rem) ->
                  Stack.push (v, w) copy_colour;
                  Interference.merge_nodes w v g, Register.S.remove w rem
                ) prefs (graph, rem)
            in
            coalesce graph rem
          end
      end
  in

  let min_degree_node graph =
    Register.M.fold (fun v _ m_n ->
        let arcs = Register.M.find v graph in
        let deg = Register.S.cardinal arcs.intfs in
        match m_n with
        | None ->
           Some (v, arcs, deg)
        | Some (_, _, d) when deg < d ->
           Some (v, arcs, deg)
        | m_s ->
           m_s
      ) graph None
  in

  let colour_node sp_colours graph v =
    let min_possible_colour constraints =
      try 
        IntSet.fold (fun c n ->
            if n < c then raise (Found_spilt n) else n + 1
          ) constraints 0
      with Found_spilt n -> n
    in
    let neigh_colours v =
      let neighs = (Register.M.find v graph).intfs in
      Register.S.fold (fun w cols ->
          try
            let c = Register.M.find w sp_colours in
            IntSet.add c cols
          with Not_found -> cols
        ) neighs IntSet.empty
    in
    let v_c = min_possible_colour (neigh_colours v) in
    n_locals := max (succ v_c) !n_locals;
    Register.M.add v v_c sp_colours
  in

  let simplify graph =
    let curr_g = ref graph in
    let to_colour = Stack.create () in
    while not (Register.M.is_empty !curr_g) do
      let min_node, neighs =
        match min_degree_node !curr_g with | Some (v, arcs, _) -> v, arcs | None -> assert false in
      curr_g := Interference.remove_node min_node neighs !curr_g;
      Stack.push min_node to_colour
    done;
    let sp_colours = ref Register.M.empty in
    while not (Stack.is_empty to_colour) do
      let v = Stack.pop to_colour in
      sp_colours := colour_node !sp_colours graph v;
    done;
    !sp_colours
  in

  let combine_colours =
    Register.M.fold (fun sp n col ->
        Register.M.add sp (Spilt (Utils.word_size * (n - !n_locals))) col
      )
  in

  let colour_coalesced sp_colours =
    let curr_sc = ref sp_colours in
    while not (Stack.is_empty copy_colour) do
      let col, uncol = Stack.pop copy_colour in
      let sp = Register.M.find col !curr_sc in
      curr_sc := Register.M.add uncol sp !curr_sc
    done;
    !curr_sc
  in

  let colour_heap_regs =
    Register.M.fold (fun _ rxs col ->
        incr n_locals;
        let stack_loc = -Utils.word_size * !n_locals in
        fst (
            List.fold_left (fun (col, n) hr ->
                Register.M.add hr (Heap (stack_loc, n)) col, n + Utils.word_size
              ) (col, 0) rxs
          )
      ) heap_map
  in
  
  let graph, have_prefs =
    Register.S.fold (fun r (g, hp) ->
        let arcs, has_prefs = reduce_arcs (Register.M.find r graph) in
        Register.M.add r arcs g, if has_prefs then Register.S.add r hp else hp
      ) spilt (Register.M.empty, Register.S.empty)
  in

  let c = combine_colours (simplify (coalesce graph have_prefs)) colouring in
  let c = colour_coalesced c in
  let c = colour_heap_regs c
  in
  
  c, !n_locals
  
  
let alloc_registers mach_regs heap_regs graph =

  let k = Register.S.cardinal mach_regs in
  let graph =
    Register.S.fold (fun r g ->
        try
          let arcs = Register.M.find r g in
          let other_rs = Register.S.remove r mach_regs in
          Register.M.add r { arcs with intfs = Register.S.union arcs.intfs other_rs } g
        with Not_found -> g
      ) mach_regs graph
  in
  
  let usage_counter =
    let incr_counter v map =
      let c =
        try Register.M.find v map
        with Not_found -> 0
      in
      Register.M.add v (c+1) map
    in
    Register.M.fold (fun v arcs u_c ->
        let u_c = Register.S.fold incr_counter arcs.prefs u_c in
        Register.S.fold incr_counter arcs.intfs u_c
      ) graph Register.M.empty
  in

  let initial_degrees =
    Register.M.map (fun arcs ->
        Register.S.cardinal arcs.intfs
      ) graph
  in

  let is_machine_register r =
    Register.S.mem r mach_regs
  in

  let min_cost_node graph =
    let min_node =
      Register.M.fold (fun v _ min_n -> (* no spill on machine registers *)
          if is_machine_register v then min_n
          else begin
              let cost =
                float_of_int (Register.M.find v usage_counter)
                /. (float_of_int (Register.M.find v initial_degrees))
              in
              match min_n with
              | None ->
                 Some (v, cost)
              | Some (_, c) when cost < c ->
                 Some (v, cost)
              | m_n ->
                 m_n
            end
        ) graph None
    in match min_node with
       | Some (v, _) ->
          v
       | None ->
          assert false 
  in

  let colour_node colours v arcs =
    let avail_colours =
      Register.S.fold (fun w avail_cols ->
          try 
            match Register.M.find w colours with
            | Spilt _ | Heap _ -> (* spilt/heap nodes do not appear here yet *)
               assert false
            | Reg r ->
               Register.S.remove r avail_cols
          with Not_found -> avail_cols
        ) arcs.intfs mach_regs
    in
    try 
      let v_c = Register.S.choose avail_colours in
      (* Format.printf "Coloured %a with %a\n" Register.string_of_reg v Register.string_of_reg  v_c; *)
      Register.M.add v (Reg v_c) colours
    with Not_found ->
      raise No_possible_colour
  in

  let george_criterion graph =
    let check_couple v2 v2_neighs v2_is_mach_reg v1 =
      let v1_neighs = (Register.M.find v1 graph).intfs in
      Register.S.iter (fun w ->
          if (is_machine_register w <> v2_is_mach_reg || Interference.degree_of w graph >= k)
             && not (Register.S.mem w v2_neighs)
          then raise Bad_pref_arc
        ) v1_neighs
    in
    let check_node v2 arcs =
      let is_mach_reg = is_machine_register v2 in
      let neighs = arcs.prefs in
      Register.S.iter (fun v1 -> (* the node to be merged/deleted MUST be a pseudo-register *)
          if not (is_machine_register v1) then
            try check_couple v2 arcs.intfs is_mach_reg v1; raise (Good_pref_arc (v1, v2))
            with Bad_pref_arc -> ()
        ) neighs
    in
    Register.M.iter check_node graph
  in

  let min_pref_disconnected_node graph =
    Register.M.fold (fun v arcs min_n -> (* no simplify on machine registers *)
        if is_machine_register v || not (Register.S.is_empty arcs.prefs) then min_n
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
      ) graph None
  in

  let george_appel colours spilt_regs =
    let spilt = ref spilt_regs in
    let curr_g = ref graph in
    let curr_c = ref colours in
    let number_of_regs = ref (Register.M.cardinal graph) in
    let stack = Stack.create () in
    Stack.push Simplify stack;
    while not (Stack.is_empty stack) do
      match Stack.pop stack with
      | Simplify ->
         begin
           (* Format.printf "Action #1: simplify\n"; *)
           match min_pref_disconnected_node !curr_g with
           | None ->
              Stack.push Coalesce stack
           | Some (v, _) ->
              Stack.push (Select v) stack
         end
      | Select v ->
         begin
           (* Format.printf "Action #2: select\n";
            * Format.printf "Deleted register %a\n" Register.string_of_reg v; *)
           let arcs = Register.M.find v !curr_g in
           Stack.push (Colour_node (v, arcs)) stack;
           curr_g := Interference.remove_node v arcs !curr_g;
           decr number_of_regs;
           Stack.push Simplify stack
         end
      | Coalesce ->
         begin
           (* Format.printf "Action #3: coalesce\n"; *)
           try
             george_criterion !curr_g;
             Stack.push Freeze stack
           with Good_pref_arc (v1, v2) ->
             (* Format.printf "Merged register %a into register %a\n"
              *   Register.string_of_reg v1 Register.string_of_reg v2; *)
             curr_g := Interference.merge_nodes v1 v2 !curr_g;
             decr number_of_regs;
             Stack.push (Copy_colour (v2, v1)) stack;
             Stack.push Simplify stack
         end
      | Freeze ->
         begin
           (* Format.printf "Action #4: freeze\n"; *)
           try
             Register.M.iter (fun v arcs -> (* no freeze on machine registers *)
                 if not (is_machine_register v) && not (Register.S.is_empty arcs.prefs)
                    && Register.S.cardinal arcs.intfs < k then raise (Found_node (v, arcs))
               ) !curr_g;
             Stack.push Spill stack
           with Found_node (v, arcs) ->
             (* Format.printf "Froze register %a\n" Register.string_of_reg v; *)
             curr_g := Register.S.fold (Interference.remove_pref_arc v) arcs.prefs !curr_g;
             curr_g := Register.M.add v { arcs with prefs = Register.S.empty } !curr_g;
             Stack.push Simplify stack
         end
      | Spill ->
         begin
           (* Format.printf "Action #5: spill\n"; *)
           if !number_of_regs > k then (* g is "empty" iff there remain only machine registers in it *)
             let v = min_cost_node !curr_g in
             Stack.push (Select v) stack
         end
      | Colour_node (v, arcs) ->
         begin
           (* Format.printf "Action #6: colour node %a\n" Register.string_of_reg v; *)
           try
             if not (Register.M.mem v !curr_c) then curr_c := colour_node !curr_c v arcs
           with No_possible_colour ->
             (* Format.printf "Spilt register %a\n" Register.string_of_reg v; *)
             spilt := Register.S.add v !spilt
         end
      | Copy_colour (col, uncol) ->
         (* Format.printf "Action #7: copy colour\n"; *)
         try
           let c = Register.M.find col !curr_c in
           (* Format.printf "Coloured %a with %a\n" Register.string_of_reg uncol print_colour c; *)
           curr_c := Register.M.add uncol c !curr_c
         with Not_found ->
           (* Format.printf "Spilt register %a\n" Register.string_of_reg uncol; *)
           spilt := Register.S.add uncol !spilt
    done;
    !spilt, !curr_c
  in
  
  let colours =
    Register.S.fold (fun r c ->
        Register.M.add r (Reg r) c
      ) mach_regs Register.M.empty
  in
  
  let heap_regs, heap_map =
    List.fold_left (fun (sp, m) rxs ->
        List.fold_left (fun sp r ->
            Register.S.add r sp
          ) sp rxs, Register.M.add (List.hd rxs) rxs m
      ) (Register.S.empty, Register.M.empty) heap_regs
  in

  let spilt_regs, colouring = george_appel colours heap_regs in
  reduce_spilt (Register.S.diff spilt_regs heap_regs) colouring graph heap_map

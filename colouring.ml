
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
exception Found_colour of colour
exception Found_spilled of int
exception No_possible_colour

let print_colour fmt = function
  | Reg mr ->
     Format.fprintf fmt "reg %a" Register.string_of_reg mr
  | Spilled n ->
     Format.fprintf fmt "spilled %d" n
               

let reduce_spilled (spilled, colouring) graph spilled_map =
  
  let copy_colour = Stack.create () in
  let n_locals = ref 0
  in

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
              Register.S.fold (fun w (g, rem) ->
                  Stack.push (v, w) copy_colour;
                  Interference.merge_nodes w v g, Register.S.remove w rem
                ) prefs (g, rem)
            in
            coalesce g rem
          end
      end
  in

  let min_degree_node g =
    Register.M.fold (fun v _ m_n ->
        let arcs = Register.M.find v g in
        let deg = Register.S.cardinal arcs.intfs in
        match m_n with
        | None ->
           Some (v, arcs, deg)
        | Some (_, _, d) when deg < d ->
           Some (v, arcs, deg)
        | m_s ->
           m_s
      ) g None
  in

  let colour_node sc g v =
    let min_possible_colour constraints =
      IntSet.fold (fun c n ->
          if n < c then raise (Found_spilled n) else n + 1
        ) constraints 0
    in
    let possible_colours constraints =
      let rec add i n set =
        if i >= n then set
        else add (succ i) n (IntSet.add i set)
      in
      snd (
          IntSet.fold (fun c (prev_c, set) ->
              c, add (succ prev_c) c set
            ) constraints (-1, IntSet.empty)
        )
    in
    let neigh_colours v =
      let neighs = (Register.M.find v g).intfs in
      Register.S.fold (fun w colours ->
          try
            let c = Register.M.find w sc in
            IntSet.add c colours
          with Not_found -> colours
        ) neighs IntSet.empty
    in
    try
      let contigs = Register.M.find v spilled_map in
      let constraints = List.map neigh_colours contigs in
      let fst_constr, rem_constr = match constraints with | x :: xs -> x, xs | [] -> assert false in
      let fst_allowed = possible_colours fst_constr in
      let fst_colour =
        try
          IntSet.iter (fun n ->
              try
                let n' = ref (succ n) in
                List.iter (fun constr -> (* check incompatibilities *)
                    if !n' >= !n_locals then raise (Found_spilled n);
                    if IntSet.mem !n' constr then raise No_possible_colour;
                    incr n'
                  ) rem_constr;
                raise (Found_spilled n)
              with No_possible_colour -> ()
            ) fst_allowed;
          !n_locals
        with Found_spilled n -> n
      in
      let sc, next_colour =
        List.fold_left (fun (sc, v_c) v ->
            Register.M.add v v_c sc, succ v_c
          ) (sc, fst_colour) contigs
      in
      n_locals := max next_colour !n_locals;
      sc
    with Not_found ->
      let v_c = min_possible_colour (neigh_colours v) in
      n_locals := max (succ v_c) !n_locals;
      Register.M.add v v_c sc
  in

  let simplify g =
    let curr_g = ref g in
    let to_colour = Stack.create () in
    while not (Register.M.is_empty !curr_g) do
      let min_node, neighs =
        match min_degree_node !curr_g with | Some (v, arcs, _) -> v, arcs | None -> assert false in
      curr_g := Interference.remove_node min_node neighs !curr_g;
      Stack.push min_node to_colour
    done;
    let sc = ref Register.M.empty in
    while not (Stack.is_empty to_colour) do
      let v = Stack.pop to_colour in
      sc := colour_node !sc g v;
    done;
    !sc
  in

  let combine_colourings =
    Register.M.fold (fun sp n col ->
        Register.M.add sp (Spilled (Utils.word_size * (n - !n_locals))) col
      )
  in

  let colour_coalesced c =
    let curr_c = ref c in
    while not (Stack.is_empty copy_colour) do
      let col, uncol = Stack.pop copy_colour in
      let sp = Register.M.find col !curr_c in
      curr_c := Register.M.add uncol sp !curr_c
    done;
    !curr_c
  in

  let g, have_prefs =
    Register.S.fold (fun r (g', hp) ->
        let arcs', has_prefs = reduce_arcs (Register.M.find r graph) in
        Register.M.add r arcs' g', if has_prefs then Register.S.add r hp else hp
      ) spilled (Register.M.empty, Register.S.empty)
  in

  let c = colour_coalesced (combine_colourings (simplify (coalesce g have_prefs)) colouring) in
  c, !n_locals
  
  
let alloc_registers mach_regs spilled_regs g =

  let k = Register.S.cardinal mach_regs in
  let g =
    Register.S.fold (fun r g' ->
        let other_rs = Register.S.remove r mach_regs in
        let arcs = Register.M.find r g' in
        Register.M.add r { arcs with intfs = Register.S.union arcs.intfs other_rs } g'
      ) mach_regs g
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
      ) g Register.M.empty
  in

  let initial_degrees =
    Register.M.map (fun arcs ->
        Register.S.cardinal arcs.intfs
      ) g
  in

  let is_machine_register r =
    Register.S.mem r mach_regs
  in

  let min_cost_node g =
    let min_node =
      Register.M.fold (fun v _ min_n ->
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
        ) g None
    in match min_node with
       | Some (v, _) ->
          v
       | None ->
          assert false 
  in

  let colour_node c v arcs =
    let poss_colours =
      Register.S.fold (fun w colours ->
          match Register.M.find w c with
          | Spilled _ ->
             assert false
          | Reg r ->
             Register.S.remove r colours
        ) arcs.intfs mach_regs
    in
    try 
      let v_c = Register.S.choose poss_colours in
      (* Format.printf "Coloured %a with %a\n" Register.string_of_reg v Register.string_of_reg  v_c; *)
      Register.M.add v (Reg v_c) c
    with Not_found ->
      raise No_possible_colour
  in

  let george_criterion g =
    let check_couple v2 v2_neighs v2_is_mach_reg v1 =
      let v1_neighs = (Register.M.find v1 g).intfs in
      Register.S.iter (fun w ->
          if (is_machine_register w <> v2_is_mach_reg || Interference.degree_of w g >= k)
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
    Register.M.iter check_node g
  in

  let min_pref_disconnected_node g =
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
      ) g None
  in

  let george_appel c spilled_regs =
    let spilled = ref spilled_regs in
    let curr_g = ref g in
    let curr_c = ref c in
    let number_of_regs = ref (Register.M.cardinal g) in
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
             Register.M.iter (fun v arcs ->
                 if not (Register.S.is_empty arcs.prefs) && Register.S.cardinal arcs.intfs < k
                 then raise (Found_node (v, arcs))
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
           (* Format.printf "Action #6: colour node\n"; *)
           try
             if not (Register.M.mem v !curr_c) then curr_c := colour_node !curr_c v arcs
           with No_possible_colour ->
             (* Format.printf "Spilled register %a\n" Register.string_of_reg v; *)
             spilled := Register.S.add v !spilled
         end
      | Copy_colour (col, uncol) ->
         (* Format.printf "Action #7: copy colour\n"; *)
         try
           let c = Register.M.find col !curr_c in
           (* Format.printf "Coloured %a with %a\n" Register.string_of_reg uncol print_colour c;*)
           curr_c := Register.M.add uncol c !curr_c
         with Not_found ->
           spilled := Register.S.add uncol !spilled
    done;
    !spilled, !curr_c
  in
  
  let c =
    Register.S.fold (fun r c ->
        Register.M.add r (Reg r) c
      ) mach_regs Register.M.empty
  in
  let spilled_regs, spilled_map =
    let len_gt_1 = function
      | _ :: _ :: _ ->
         true
      | _ ->
         false
    in
    List.fold_left (fun (sp, m) rxs ->
        let add_to_map = len_gt_1 rxs in
        List.fold_left (fun (sp, m) r ->
            Register.S.add r sp,
            if add_to_map then Register.M.add r rxs m else m
          ) (sp, m) rxs
      ) (Register.S.empty, Register.M.empty) spilled_regs
  in

  reduce_spilled (george_appel c spilled_regs) g spilled_map

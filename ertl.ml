
open Register
open Rtltree
open Ertltree

let graph = ref Label.M.empty
let mem_locals_set = ref Register.S.empty
let mem_locals = ref []
                   
let generate a =
  let l = Label.fresh () in
  graph := Label.M.add l a !graph;
  l

let assoc_arguments args =
  let rec assoc (reg, stack) = function
    | [], _ ->
       List.rev reg, List.rev stack
    | ra :: args, [] ->
       assoc (reg, ra :: stack) (args, [])
    | ra :: args, rp :: params ->
       assoc ((ra, rp) :: reg, stack) (args, params) 
  in
  assoc ([], []) (args, Register.parameters)

let move src dst l =
  generate (Embinop (Mmov, src, dst, l))

let pop_param r l =
  generate (Epop_param (r, l))

let push_param r l =
  generate (Epush_param (r, l))

let get_param ofs r l =
  generate (Eget_param (ofs, r, l))

(* Functions' return values convention:
 *** if only one 8B result, then in %rax
 *** otherwise, all the results on the stack and the first one is at the lowest address *)
let move_return_call l = function
  | [] ->
     l
  | [r] ->
     move Register.rax r l
  | retrs ->
     List.fold_right pop_param retrs l

let move_return_def l = function
  | [] ->
     l
  | [r] ->
     move r Register.rax l
  | retrs ->
     let ofs = Utils.word_size lsl 1 in
     fst (List.fold_left (fun (l, ofs) r -> get_param ofs r l, ofs + Utils.word_size) (l, ofs) retrs)

let instr = function
  | Rtltree.Eint (n, r, l) ->
     Eint (n, r, l)
  | Rtltree.Estring (s, r, l) ->
     Estring (s, r, l)
  | Rtltree.Ebool (b, r, l) ->
     Ebool (b, r, l)
  | Rtltree.Elea_local (rxs, ofs, dst, l) ->
     (* registers rxs belong to f.locals *)
     let fst_rx = List.hd rxs in
     if not (Register.S.mem fst_rx !mem_locals_set) then begin
         mem_locals := rxs :: !mem_locals;
         mem_locals_set := List.fold_left (fun set r -> Register.S.add r set) !mem_locals_set rxs
       end;
     Elea_local (fst_rx, ofs, dst, l)
  | Rtltree.Elea (src, ofs, dst, l) -> 
     Elea (src, ofs, dst, l)
  | Rtltree.Eload (srcs, ofs, dsts, l) ->
     let instr_opt, _, _ =
       List.fold_left2 (fun (_, l, n) src dst ->
           let i = Eload (src, n, dst, l) in
           Some i, generate i, n + Utils.word_size
         ) (None, l, ofs) srcs dsts
     in
     begin match instr_opt with Some i -> i | None -> assert false end
  | Rtltree.Estore (src, dst, ofs, l) ->
     Estore (src, dst, ofs, l)
  | Rtltree.Emubranch (op, r, true_l, false_l) ->
     Emubranch (op, r, true_l, false_l)
  | Rtltree.Embbranch (op, r_arg, l_arg, true_l, false_l) ->
     Embbranch (op, r_arg, l_arg, true_l, false_l)
  | Rtltree.Egoto l ->
     Egoto l
  | Rtltree.Emunop (Midivil n, r, l) ->
     Embinop (Mmov, r, Register.rax, generate (
     Emunop  (Midivil n, Register.rax, generate (
     Embinop (Mmov, Register.rax, r, l)))))
  | Rtltree.Emunop (Midivir n, r, l) ->
     Eint    (n, Register.rax, generate (
     Embinop (Midiv, r, Register.rax, generate (
     Embinop (Mmov, Register.rax, r, l)))))
  | Rtltree.Emunop (Mmodil n, r, l) ->
     Embinop (Mmov, r, Register.rax, generate (
     Emunop  (Midivil n, Register.rax, generate (
     Embinop (Mmov, Register.rdx, r, l)))))
  | Rtltree.Emunop (Mmodir n, r, l) ->
     Eint    (n, Register.rax, generate (
     Embinop (Midiv, r, Register.rax, generate (
     Embinop (Mmov, Register.rdx, r, l)))))
  | Rtltree.Emunop (op, r, l) ->
     Emunop (op, r, l)
  | Rtltree.Embinop (Midiv, r1, r2, l) ->
     Embinop (Mmov, r2, Register.rax, generate (
     Embinop (Midiv, r1, Register.rax, generate (
     Embinop (Mmov, Register.rax, r2, l)))))
  | Rtltree.Embinop (Mmod, r1, r2, l) ->
     Embinop (Mmov, r2, Register.rax, generate (
     Embinop (Midiv, r1, Register.rax, generate (
     Embinop (Mmov, Register.rdx, r2, l)))))
  | Rtltree.Embinop (op, r1, r2, l) ->
     Embinop (op, r1, r2, l)
  | Rtltree.Emalloc (r, n, l) ->
     Eint (n, Register.rdi, generate (
     Ecall ("malloc", 1, generate (
     Embinop (Mmov, Register.rax, r, l)))))
  | Rtltree.Ecall (res, f, actuals, l) ->
     let size_res = Utils.word_size * List.length res in
     let act_param, stack = assoc_arguments actuals in
     let n = List.length act_param in
     let size_stack = List.length stack in
     let l = if size_stack = 0 then l
             else generate (Efree_stack (Int32.of_int (Utils.word_size * List.length stack), l))
     in
     let l = generate (Ecall (f, n, move_return_call l res)) in
     (* reserve stack space for results only if more than one *)
     let l = if size_res > Utils.word_size then generate (Ealloc_stack (Int32.of_int size_res, l))
             else l
     in
     let l = List.fold_right (fun r l -> push_param r l) stack l in
     let l = List.fold_right (fun (a, r) l -> move a r l) act_param l in
     Egoto l
  | Rtltree.Eprint (regs, l) ->
     let arg_param, stack = assoc_arguments regs in
     let n = List.length arg_param in
     let l = generate (Ecall ("printf", n, l)) in
     let l = generate (Embinop (Mxor, Register.rax, Register.rax, l)) in (* set AL to 0 *)
     let l = List.fold_right (fun r l -> push_param r l) stack l in
     let l = List.fold_right (fun (a, r) l -> move a r l) arg_param l in
     Egoto l

let fun_entry saved formals entry res_on_stack =
  let form_param, stack = assoc_arguments formals in
  let ofs = Utils.word_size * (2 + res_on_stack) in
  let l, _ = List.fold_right (* get first the first stored argument *)
            (fun f (l, ofs) -> get_param ofs f l, ofs + Utils.word_size) stack (entry, ofs)
  in
  let l = List.fold_right (fun (f, r) l -> move r f l) form_param l in
  let l = List.fold_right (fun (s, r) l -> move r s l) saved l in
  generate (Ealloc_frame l)

let fun_exit saved retrs exitl =
  let l = generate (Efree_frame (generate Ereturn)) in
  let l = List.fold_right (fun (s, r) l -> move s r l) saved l in
  let l = move_return_def l retrs in
  graph := Label.M.add exitl (Egoto l) !graph
  
let funct (f:Rtltree.decl_fun) =
  Label.M.iter (fun l i -> let i = instr i in graph := Label.M.add l i !graph) f.body; 
  let saved = List.map (fun r -> Register.fresh (), r) Register.callee_saved in
  let n_res = List.length f.result in
  let res_on_stack = if n_res > 1 then n_res else 0 in 
  let entry = fun_entry saved f.formals f.entry res_on_stack in
  fun_exit saved f.result f.exit_;
  let body = !graph in
  let stored_locals = !mem_locals in
  graph := Label.M.empty;
  mem_locals_set := Register.S.empty;
  mem_locals := [];
  { formals = List.length f.formals; locals = f.locals; stored_locals; entry; body }
    
let file =
  Asg.Smap.map funct

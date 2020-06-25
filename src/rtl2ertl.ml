
open Ertl

let graph = ref Label.M.empty
let heap_locals_set = ref Register.S.empty
let heap_locals = ref []
                   
let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l

let assoc_arguments args =
  let rec assoc (reg, stack) = function
    | [], _ -> List.rev reg, List.rev stack
    | ra :: args, [] -> assoc (reg, ra :: stack) (args, [])
    | ra :: args, rp :: params -> assoc ((ra, rp) :: reg, stack) (args, params)
  in
  assoc ([], []) (args, Register.parameters)

let translate_munop = function
  | Isl.Mnot -> Mnot
  | Isl.Mneg -> Mneg
  | Isl.Maddi n -> Maddi n
  | Isl.Mimuli n -> Mimuli n
  | Isl.Minc -> Minc
  | Isl.Mdec -> Mdec
  | Isl.Midivil _  | Isl.Midivir _
  | Isl.Mmodil _ | Isl.Mmodir _ -> assert false
  | Isl.Msetei n -> Msetei n
  | Isl.Msetnei n -> Msetnei n
  | Isl.Msetgi n -> Msetgi n
  | Isl.Msetgei n -> Msetgei n
  | Isl.Msetli n -> Msetli n
  | Isl.Msetlei n -> Msetlei n

let translate_binop = function
  | Isl.Madd -> Madd
  | Isl.Msub -> Msub
  | Isl.Mimul -> Mimul
  | Isl.Midiv | Isl.Mmod -> assert false
  | Isl.Mxor -> Mxor
  | Isl.Msete -> Msete
  | Isl.Msetne -> Msetne
  | Isl.Msetg -> Msetg
  | Isl.Msetge -> Msetge
  | Isl.Msetl -> Msetl
  | Isl.Msetle -> Msetle
  | Isl.Mmov -> Mmov

let move src dst l =
  generate (Imbinop (Mmov, src, dst, l))

let pop_param r l =
  generate (Ipop_param (r, l))

let push_param r l =
  generate (Ipush_param (r, l))

let get_param ofs r l =
  generate (Iget_param (ofs, r, l))

let set_result r ofs l =
  generate (Iset_result (r, ofs, l))

(* Functions' return values convention:
 *** if only one 8 B result, then in %rax
 *** otherwise, all the results on the stack and the first one is at the lowest address *)
let move_return_call l = function
  | [] -> l
  | [r] -> move Register.rax r l
  | retrs -> List.fold_right pop_param retrs l

let move_return_def l = function
  | [] ->
     l
  | [r] ->
     move r Register.rax l
  | retrs ->
     let ofs = Utils.word_size lsl 1 in
     fst (List.fold_left (fun (l, ofs) r ->
              set_result r ofs l, ofs + Utils.word_size
            ) (l, ofs) retrs)

let instr = function
  | Rtl.Iint (n, r, l) ->
     Iint (n, r, l)
  | Rtl.Istring (s, r, l) ->
     Istring (s, r, l)
  | Rtl.Ibool (b, r, l) ->
     Ibool (b, r, l)
  | Rtl.Imalloc (r, n, l) ->
     Iint (Int64.of_int32 n, Register.rdi, generate (
     Icall ("malloc", 1, generate (
     Imbinop (Mmov, Register.rax, r, l)))))
  | Rtl.Ilea_local (rxs, ofs, dst, l) ->
     (* registers rxs belong to f.locals *)
     let fst_rx = List.hd rxs in
     if not (Register.S.mem fst_rx !heap_locals_set) then begin
         heap_locals := rxs :: !heap_locals;
         heap_locals_set := List.fold_left (fun set r ->
                               Register.S.add r set
                             ) !heap_locals_set rxs
       end;
     Ilea_local (fst_rx, ofs, dst, l)
  | Rtl.Ilea (src, ofs, dst, l) -> 
     Ilea (src, ofs, dst, l)
  | Rtl.Iload (src, ofs, dst, l) ->
     Iload (src, ofs, dst, l)
  | Rtl.Istore (src, dst, ofs, l) ->
     Istore (src, dst, ofs, l)
  | Rtl.Icall (res, f, actuals, l) ->
     let size_res = Utils.word_size * List.length res in
     let act_param, stack = assoc_arguments actuals in
     let n = List.length act_param in
     let size_stack = List.length stack in
     let l =
       if size_stack = 0 then l
       else generate (Ifree_stack (Int32.of_int (Utils.word_size * List.length stack), l))
     in
     let l = generate (Icall (f, n, move_return_call l res)) in
     (* reserve stack space for results only if more than one *)
     let l =
       if size_res > Utils.word_size then generate (Ialloc_stack (Int32.of_int size_res, l))
       else l
     in
     let l = List.fold_right (fun r l -> push_param r l) stack l in
     let l = List.fold_right (fun (a, r) l -> move a r l) act_param l in
     Igoto l
  | Rtl.Iprint (regs, l) ->
     let arg_param, stack = assoc_arguments regs in
     let n = List.length arg_param in
     let l = generate (Icall ("printf", n, l)) in
     let l = generate (Imbinop (Mxor, Register.rax, Register.rax, l)) in (* set AL to 0 *)
     let l = List.fold_right (fun r l -> push_param r l) stack l in
     let l = List.fold_right (fun (a, r) l -> move a r l) arg_param l in
     Igoto l
  | Rtl.Imunop (Midivil n, r, l) ->
     Imbinop   (Mmov, r, Register.rax, generate (
     Iidiv_imm (n, generate (
     Imbinop   (Mmov, Register.rax, r, l)))))
  | Rtl.Imunop (Midivir n, r, l) ->
     Iint      (n, Register.rax, generate (
     Iidiv     (r, generate (
     Imbinop   (Mmov, Register.rax, r, l)))))
  | Rtl.Imunop (Mmodil n, r, l) ->
     Imbinop   (Mmov, r, Register.rax, generate (
     Iidiv_imm (n, generate (
     Imbinop   (Mmov, Register.rdx, r, l)))))
  | Rtl.Imunop (Mmodir n, r, l) ->
     Iint      (n, Register.rax, generate (
     Iidiv     (r, generate (
     Imbinop   (Mmov, Register.rdx, r, l)))))
  | Rtl.Imunop (op, r, l) ->
     Imunop    (translate_munop op, r, l)
  | Rtl.Imbinop (Midiv, r1, r2, l) ->
     Imbinop   (Mmov, r2, Register.rax, generate (
     Iidiv     (r1, generate (
     Imbinop   (Mmov, Register.rax, r2, l)))))  
  | Rtl.Iinc_dec (op, r, ofs, l) -> 
     Iinc_dec  (op, r, ofs, l)
  | Rtl.Imbinop (Mmod, r1, r2, l) ->
     Imbinop   (Mmov, r2, Register.rax, generate (
     Iidiv     (r1, generate (
     Imbinop   (Mmov, Register.rdx, r2, l)))))
  | Rtl.Imbinop (op, r1, r2, l) ->
     Imbinop   (translate_binop op, r1, r2, l)
  | Rtl.Imubranch (op, r, true_l, false_l) ->
     Imubranch (op, r, true_l, false_l)
  | Rtl.Imbbranch (op, r_arg, l_arg, true_l, false_l) ->
     Imbbranch (op, r_arg, l_arg, true_l, false_l)
  | Rtl.Igoto l ->
     Igoto l

let fun_entry saved formals entry res_on_stack =
  let form_param, stack = assoc_arguments formals in
  let ofs = Utils.word_size * (2 + res_on_stack) in
  let l, _ = List.fold_right (* get first the first stored argument *)
            (fun f (l, ofs) -> get_param ofs f l, ofs + Utils.word_size) stack (entry, ofs)
  in
  let l = List.fold_right (fun (f, r) l -> move r f l) form_param l in
  let l = List.fold_right (fun (s, r) l -> move r s l) saved l in
  generate (Ialloc_frame l)

let fun_exit fname saved retrs exitl =
  let l = generate Ireturn in
  let l = if fname <> "main" then l
          else generate (Imbinop (Mxor, Register.rax, Register.rax, l)) (* exit *)
  in
  let l = generate (Ifree_frame l) in
  let l = List.fold_right (fun (s, r) l -> move s r l) saved l in
  let l = move_return_def l retrs in
  graph := Label.M.add exitl (Igoto l) !graph
  
let funct fname (f:Rtl.rfundef) =
  Label.M.iter (fun l i -> let i = instr i in graph := Label.M.add l i !graph) f.body; 
  let saved = List.map (fun r -> Register.fresh (), r) Register.callee_saved in
  let n_res = List.length f.result in
  let res_on_stack = if n_res > 1 then n_res else 0 in 
  let entry = fun_entry saved f.formals f.entry res_on_stack in
  fun_exit fname saved f.result f.exit_;
  let body = !graph in
  let stored_locals = !heap_locals in
  graph := Label.M.empty;
  heap_locals_set := Register.S.empty;
  heap_locals := [];
  { formals = List.length f.formals; locals = f.locals; stored_locals; entry; body }
    
let programme (p:Rtl.rprogramme) =
  Utils.Smap.mapi funct p.functions


open Ltltree

let graph = ref Label.M.empty
let heap_regs = ref []
   
let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l
   
let lookup colours r =
  Register.M.find r colours
   
let write colours r l =
  match lookup colours r with
  | Colouring.Reg mr ->
     mr, l
  | Colouring.Spilt n ->
     Register.tmp1, generate (Imbinop (Mmov, Reg Register.tmp1, Spilt n, l))
  | Colouring.Heap _ ->
     assert false

let read colours r mr f =
  match lookup colours r with
  | Colouring.Reg mr -> f mr
  | Colouring.Spilt n -> Imbinop (Mmov, Spilt n, Reg mr, generate (f mr))
  | Colouring.Heap _ -> assert false
    
let read1 colours r f =
  read colours r Register.tmp1 f

let read2 colours r1 r2 f =
  read colours r1 Register.tmp1 (fun mr1 ->
      read colours r2 Register.tmp2 (fun mr2 -> f mr1 mr2)
    )
  
let instr colours frame = function
  | Ertltree.Iint (n, r, l) ->
     Iint (n, lookup colours r, l)
  | Ertltree.Istring (s, r, l) ->
     let mr, l = write colours r l in
     Istring (s, mr, l)
  | Ertltree.Ibool (b, r, l) ->
     Ibool (b, lookup colours r, l)
  | Ertltree.Ilea_local (rx, ofs, dst, l) ->
     let stack_ofs, heap_ofs =
       match lookup colours rx with | Heap (s, h) -> s, h | _ -> assert false
     in
     let dst_mr, l = write colours dst l in
     Iload (Register.rbp, stack_ofs, Register.tmp1, generate (
     Ilea  (Register.tmp1, heap_ofs, dst_mr, l)))
  | Ertltree.Ilea (src, ofs, dst, l) ->
     read1 colours src (fun src_mr ->
         let dst_mr, l = write colours dst l in
         Ilea (src_mr, ofs, dst_mr, l)
       )
  | Ertltree.Iload (src, ofs, dst, l) ->
     read1 colours src (fun src_mr ->
         let dst_mr, l = write colours dst l in
         Iload (src_mr, ofs, dst_mr, l)
       )
  | Ertltree.Istore (src, dst, ofs, l) ->
     read2 colours src dst (
         fun src_mr dst_mr -> Istore (src_mr, dst_mr, ofs, l)
       )
  | Ertltree.Icall (f, n_args, l) ->
     Icall (f, l)
  | Ertltree.Imunop (op, r, l) ->
     begin
       match op, lookup colours r with
       | Mimuli _, (Spilt _ as reg) ->
          read1 colours r (fun mr ->
              Imunop (op, Reg mr, generate (
              Imbinop (Mmov, Reg mr, reg, l)))
            )
       | _, reg ->
          Imunop (op, reg, l)
     end
  | Ertltree.Iidiv_imm (n, l) ->
     Iidiv_imm (n, l)
  | Ertltree.Iidiv (r, l) ->
     Iidiv (lookup colours r, l)
  | Ertltree.Iinc_dec (op, r, ofs, l) ->
     read1 colours r (fun mr ->
         Iinc_dec (op, mr, ofs, l)
       )
  | Ertltree.Imbinop (op, src, dst, l) ->
     begin
       match op, lookup colours src, lookup colours dst with
       | Mmov, r1, r2 when r1 = r2 ->
          Igoto l
       | _, (Spilt _ as r1), (Spilt _ as r2)
       | Mimul, r1, (Spilt _ as r2) ->
          read1 colours dst (fun dst_mr ->
              Imbinop (op, r1, Reg dst_mr, generate (
              Imbinop (Mmov, Reg dst_mr, r2, l)))
            )
       | Mmov, Heap (stack_ofs, heap_ofs), r2 ->
          let dst_mr, l = write colours dst l in
          Iload (Register.rbp, stack_ofs, Register.tmp2, generate (
          Iload (Register.tmp2, heap_ofs, dst_mr, l)))
       | Mmov, r1, Heap (stack_ofs, heap_ofs) ->
          read1 colours src (fun src_mr ->
          Iload (Register.rbp, stack_ofs, Register.tmp2, generate (
          Istore (src_mr, Register.tmp2, heap_ofs, l))))
       | _, r1, r2 ->
          Imbinop (op, r1, r2, l)
     end
  | Ertltree.Imubranch (op, r, true_l, false_l) ->
     Imubranch (op, lookup colours r, true_l, false_l)
  | Ertltree.Imbbranch (op, r_arg, l_arg, true_l, false_l) ->
     Imbbranch (op, lookup colours r_arg, lookup colours l_arg, true_l, false_l)
  | Ertltree.Igoto l ->
     Igoto l
  | Ertltree.Ialloc_frame l ->
     let l =
       match !heap_regs with
       | [] ->
          l
       | _ ->
          (* tmp2 is callee-saved: no data loss due to malloc *)
          let l = generate (Imbinop (Mmov, Reg Register.tmp2, Reg Register.rdi, l)) in
          let l =
            List.fold_left (fun l rxs ->
                let ofs =
                  match lookup colours (List.hd rxs) with
                  | Heap (s, _) -> s
                  | _ -> assert false
                in
                let l = generate (Istore (Register.rax, Register.rbp, ofs, l)) in
                let l = generate (Icall ("malloc", l)) in
                generate (Iint (Int64.of_int (Utils.word_size * List.length rxs),
                                Reg Register.rdi, l))
              ) l !heap_regs
          in
          generate (Imbinop (Mmov, Reg Register.rdi, Reg Register.tmp2, l))
     in
     (* always assign %rbp <- %rsp since %rbp could be used in multiple-return functions *)
     Ipush   (Reg Register.rbp, generate (
     Imbinop (Mmov, Reg Register.rsp, Reg Register.rbp, if frame.f_locals = 0 then l else generate (
     Imunop  (Maddi (Int64.of_int (-frame.f_locals)), Reg Register.rsp, l)))))
  | Ertltree.Ifree_frame l ->
     Imbinop (Mmov, Reg Register.rbp, Reg Register.rsp, generate (
     Ipop    (Register.rbp, l)))
  | Ertltree.Iget_param (n, r, l) ->
     let mr, l = write colours r l in
     Imbinop (Mmov, Spilt n, Reg mr, l)
  | Ertltree.Iset_result (r, ofs, l) ->
     read1 colours r (fun mr ->
         Istore (mr, Register.rbp, ofs, l)
       )
  | Ertltree.Ipush_param (r, l) ->
     Ipush (lookup colours r, l)
  | Ertltree.Ipop_param (r, l) ->
     let mr, l = write colours r l in
     Ipop (mr, l)
  | Ertltree.Ialloc_stack (n, l) ->
     Imunop (Maddi (Int64.of_int32 (Int32.neg n)), Reg Register.rsp, l)
  | Ertltree.Ifree_stack (n, l) ->
     Imunop (Maddi (Int64.of_int32 n), Reg Register.rsp, l)
  | Ertltree.Ireturn ->
     Ireturn

let funct (f:Ertltree.efundef) =
  let live = Liveness.perform_analysis f.body in
  let intf_graph = Interference.build_graph live in
  let colours, n_locals = Colouring.alloc_registers Register.allocable f.stored_locals intf_graph
  in
  heap_regs := f.stored_locals;
  let n_stack_params = max 0 (f.formals - List.length Register.parameters) in
  let frame =
    { f_params = Utils.word_size * n_stack_params; f_locals = Utils.word_size * n_locals }
  in
  Label.M.iter (fun l i ->
      let i = instr colours frame i in
      graph := Label.M.add l i !graph
    ) f.body;
  let body = !graph in
  graph := Label.M.empty;
  { entry = f.entry; body }
  
let programme =
  Asg.Smap.map funct

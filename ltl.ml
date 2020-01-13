
open Register
open Istree
open Ertltree
open Liveness
open Interference
open Colouring
open Ltltree

let graph = ref Label.M.empty
   
let generate a =
  let l = Label.fresh () in
  graph := Label.M.add l a !graph;
  l
   
let lookup colours r =
  Register.M.find r colours
   
let write colours r l =
  match lookup colours r with
  | Reg mr ->
     mr, l
  | Spilled n ->
     Register.tmp1, generate (Imbinop (Mmov, Reg Register.tmp1, Spilled n, l))

let read colours r mr f =
  match lookup colours r with
  | Reg mr ->
     f mr
  | Spilled n ->
     Imbinop (Mmov, Spilled n, Reg mr, generate (f mr))
    
let read1 colours r f =
  read colours r Register.tmp1 f

let read2 colours r1 r2 f =
  read colours r1 Register.tmp1 (
      fun mr1 ->
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
     let local_ofs = match lookup colours rx with | Spilled n -> n | Reg _ -> assert false in
     let dst_mr, l = write colours dst l in
     Ilea (Register.rbp, local_ofs + ofs, dst_mr, l)
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
       | Mimuli _, (Spilled _ as reg) ->
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
       | _, (Spilled _ as r1), (Spilled _ as r2)
       | Mimul, r1, (Spilled _ as r2) ->
          read1 colours dst (fun dst_mr ->
              Imbinop (op, r1, Reg dst_mr, generate (
              Imbinop (Mmov, Reg dst_mr, r2, l)))
            )
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
     (* always assign %rbp <- %rsp since %rbp could be used in multiple-return functions *)
     Ipush   (Reg rbp, generate (
     Imbinop (Mmov, Reg rsp, Reg rbp, if frame.f_locals = 0 then l else generate (
     Imunop  (Maddi (Int32.of_int (-frame.f_locals)), Reg rsp, l)))))
  | Ertltree.Ifree_frame l ->
     Imbinop (Mmov, Reg rbp, Reg rsp, generate (
     Ipop    (rbp, l)))
  | Ertltree.Iget_param (n, r, l) ->
     let mr, l = write colours r l in
     Imbinop (Mmov, Spilled n, Reg mr, l)
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
     Imunop (Maddi (Int32.neg n) , Reg rsp, l)
  | Ertltree.Ifree_stack (n, l) ->
     Imunop (Maddi n, Reg rsp, l)
  | Ertltree.Ireturn ->
     Ireturn

let funct (f:Ertltree.decl_fun) =
  let live = Liveness.perform_analysis f.body in
  let intf_graph = Interference.build_graph live in
  let colours, n_locals = Colouring.alloc_registers Register.allocable f.stored_locals intf_graph in
  let n_stack_params = max 0 (f.formals - List.length Register.parameters) in
  let frame =
    { f_params = Utils.word_size * n_stack_params; f_locals = Utils.word_size * n_locals }
  in
  Label.M.iter (fun l i -> let i = instr colours frame i in graph := Label.M.add l i !graph) f.body;
  let body = !graph in
  graph := Label.M.empty;
  { entry = f.entry; body }
  
let file =
  Asg.Smap.map funct

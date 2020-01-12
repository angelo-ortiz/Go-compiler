
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
     Register.tmp1, generate (Embinop (Mmov, Reg Register.tmp1, Spilled n, l))

let read colours r mr f =
  match lookup colours r with
  | Reg mr ->
     f mr
  | Spilled n ->
     Embinop (Mmov, Spilled n, Reg mr, generate (f mr))
    
let read1 colours r f =
  read colours r Register.tmp1 f

let read2 colours r1 r2 f =
  read colours r1 Register.tmp1 (
      fun mr1 ->
      read colours r2 Register.tmp2 (fun mr2 -> f mr1 mr2)
    )
  
let instr colours frame = function
  | Ertltree.Eint (n, r, l) ->
     Eint (n, lookup colours r, l)
  | Ertltree.Estring (s, r, l) ->
     Estring (s, lookup colours r, l)
  | Ertltree.Ebool (b, r, l) ->
     Ebool (b, lookup colours r, l)
  | Ertltree.Elea_local (rx, ofs, dst, l) ->
     let local_ofs = match lookup colours rx with | Spilled n -> n | Reg _ -> assert false in
     let dst_mr, l = write colours dst l in
     Elea (Register.rbp, local_ofs + ofs, dst_mr, l)
  | Ertltree.Elea (src, ofs, dst, l) ->
     read1 colours src (fun src_mr ->
         let dst_mr, l = write colours dst l in
         Elea (src_mr, ofs, dst_mr, l)
       )
  | Ertltree.Eload (src, ofs, dst, l) ->
     read1 colours src (fun src_mr ->
         let dst_mr, l = write colours dst l in
         Eload (src_mr, ofs, dst_mr, l)
       )
  | Ertltree.Estore (src, dst, ofs, l) ->
     read2 colours src dst (
         fun src_mr dst_mr -> Estore (src_mr, dst_mr, ofs, l)
       )
  | Ertltree.Ecall (f, n_args, l) ->
     Ecall (f, n_args, l)
  | Ertltree.Emunop (op, r, l) ->
     begin
       match op, lookup colours r with
       | Mimuli _, (Spilled _ as reg) ->
          read1 colours r (fun mr ->
              Emunop (op, Reg mr, generate (
              Embinop (Mmov, Reg mr, reg, l)))
            )
       | _, reg ->
          Emunop (op, reg, l)
     end
  | Ertltree.Embinop (op, src, dst, l) ->
     begin
       match op, lookup colours src, lookup colours dst with
       | Mmov, r1, r2 when r1 = r2 ->
          Egoto l
       | _, (Spilled _ as r1), (Spilled _ as r2)
       | Mimul, r1, (Spilled _ as r2) ->
          read1 colours dst (fun dst_mr ->
              Embinop (op, r1, Reg dst_mr, generate (
              Embinop (Mmov, Reg dst_mr, r2, l)))
            )
       | _, r1, r2 ->
          Embinop (op, r1, r2, l)
     end
  | Ertltree.Emubranch (op, r, true_l, false_l) ->
     Emubranch (op, lookup colours r, true_l, false_l)
  | Ertltree.Embbranch (op, r_arg, l_arg, true_l, false_l) ->
     Embbranch (op, lookup colours r_arg, lookup colours l_arg, true_l, false_l)
  | Ertltree.Egoto l ->
     Egoto l
  | Ertltree.Ealloc_frame l ->
     (* always assign %rbp <- %rsp since %rbp could be used in multiple-return functions *)
     Epush   (Reg rbp, generate (
     Embinop (Mmov, Reg rsp, Reg rbp, if frame.f_locals = 0 then l else generate (
     Emunop  (Maddi (Int32.of_int (-frame.f_locals)), Reg rsp, l)))))
  | Ertltree.Efree_frame l ->
     Embinop (Mmov, Reg rbp, Reg rsp, generate (
     Epop    (rbp, l)))
  | Ertltree.Eget_param (n, r, l) ->
     let mr, l = write colours r l in
     Embinop (Mmov, Spilled n, Reg mr, l)
  | Ertltree.Epush_param (r, l) ->
     Epush (lookup colours r, l)
  | Ertltree.Epop_param (r, l) ->
     let mr, l = write colours r l in
     Epop (mr, l)
  | Ertltree.Ealloc_stack (n, l) ->
     Emunop (Maddi (Int32.neg n) , Reg rsp, l)
  | Ertltree.Efree_stack (n, l) ->
     Emunop (Maddi n, Reg rsp, l)
  | Ertltree.Ereturn ->
     Ereturn

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

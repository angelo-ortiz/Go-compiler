
open Register
open Rtltree
open Ertltree

let graph = ref Label.M.empty

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

let pop_param l r =
  generate (Epop_param (r, l))

let push_param r l =
  generate (Epush_param (r, l))

let get_param r ofs l =
  generate (Eget_param (ofs, r, l))

(* Functions' return values convention: first result, if any, in %rax and
   the remaining ones on the stack *)
let move_return_call l = function
  | [] ->
     l
  | [r] ->
     move Register.rax r l
  | r :: retrs ->
     let l = List.fold_left pop_param l retrs in
     move Register.rax r l

let move_return_def l = function
  | [] ->
     l
  | [r] ->
     move r Register.rax l
  | r :: retrs ->
     let l = List.fold_right push_param retrs l in
     move r Register.rax l

let pop n l =
  if n = 0 then l
  else generate (Emunop (Maddi (Int32.of_int n), Register.rsp, l))
  
let instr = function
  | Rtltree.Eint (n, r, l) ->
     Eint (n, r, l)
  | Rtltree.Estring (s, r, l) ->
     Estring (s, r, l)
  | Rtltree.Ebool (b, r, l) ->
     Ebool (b, r, l)
  | Rtltree.Elea (src, dst, l) ->
     Elea (src, dst, l)
  | Rtltree.Eload (src, ofs, dst, l) ->
     Eload (src, ofs, dst, l)
  | Rtltree.Estore_field (src, dst, ofs, l) ->
     Estore_field (src, dst, ofs, l)
  | Rtltree.Estore_dref (src, dst, l) ->
     Estore_dref (src, dst, l)
  | Rtltree.Emubranch (op, r, true_l, false_l) ->
     Emubranch (op, r, true_l, false_l)
  | Rtltree.Embbranch (op, r_arg, l_arg, true_l, false_l) ->
     Embbranch (op, r_arg, l_arg, true_l, false_l)
  | Rtltree.Egoto l ->
     Egoto l
  | Rtltree.Emunop (Midivil n, r, l) ->
     Embinop (Mmov, r, Register.rax, generate (
     Emunop (Midivil n, Register.rax, generate (
     Embinop (Mmov, Register.rax, r, l)))))
  | Rtltree.Emunop (Midivir n, r, l) ->
     Eint (n, Register.rax, generate (
     Embinop (Midiv, r, Register.rax, generate (
     Embinop (Mmov, Register.rax, r, l)))))
  | Rtltree.Emunop (Mmodil n, r, l) ->
     Embinop (Mmov, r, Register.rax, generate (
     Emunop (Midivil n, Register.rax, generate (
     Embinop (Mmov, Register.rdx, r, l)))))
  | Rtltree.Emunop (Mmodir n, r, l) ->
     Eint (n, Register.rax, generate (
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
     Eint (Int32.of_int n, Register.rdi, generate (
     Ecall (1, "malloc", 1, generate (
     Embinop (Mmov, Register.rax, r, l)))))
  | Rtltree.Ecall (res, f, actuals, l) ->
     let act_param, stack = assoc_arguments actuals in
     let n = List.length act_param in
     let l = pop (Utils.word_size * List.length stack) l in
     let l = generate (Ecall (List.length res, f, n, move_return_call l res)) in
     let l = List.fold_right (fun r l -> push_param r l) stack l in
     let l = List.fold_right (fun (a, r) l -> move a r l) act_param l in
     Egoto l
  | Rtltree.Eprint (regs, l) ->
     let arg_param, stack = assoc_arguments regs in
     let n = List.length arg_param in
     let l = generate (Ecall (0, "printf", n, l)) in
     let l = generate (Emunop (Mxor, Register.rax, l)) in (* set AL to 0 *)
     let l = List.fold_right (fun r l -> push_param r l) stack l in
     let l = List.fold_right (fun (a, r) l -> move a r l) arg_param l in
     Egoto l

let fun_entry saved formals entry =
  let form_param, stack = assoc_arguments formals in
  let ofs = ref (Utils.word_size * (2 + List.length stack)) in (* return address *)
  let l = List.fold_left (* get first the last stored argument *)
            (fun l f -> ofs := !ofs - Utils.word_size; get_param f !ofs l) entry stack
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
  let entry = fun_entry saved f.formals f.entry in
  fun_exit saved f.result f.exit_;
  let body = !graph in
  graph := Label.M.empty;
  { formals = List.length f.formals; result = List.length f.result; locals = f.locals; entry; body }
    
let file (f:Rtltree.file) =  
  { structs = f.structs; functions = Asg.Smap.map funct f.functions }

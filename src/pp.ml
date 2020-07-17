
open Isl
open Rtl
open Ertl
open Ltl
   
let print_list fmt sep print_el l =
  let rec print fmt = function
    | [] -> ()
    | [e] -> Format.fprintf fmt "%a" print_el e
    | e :: l -> Format.fprintf fmt "%a%s%a" print_el e sep print l
  in
  print fmt l

let print_reg_set fmt set =
  Format.fprintf fmt "{";
  Register.S.iter (Format.fprintf fmt " %a " Register.print) set;
  Format.fprintf fmt "}"

let print_smap fmt (print_pair, smap) =
  Utils.Smap.iter (fun k v -> Format.fprintf fmt "%a\n" print_pair (k, v)) smap

(** {2 ISL} **)

let print_isl_munop fmt = function
  | Isl.Mnot -> Format.fprintf fmt "not"
  | Isl.Mneg -> Format.fprintf fmt "neg"
  | Isl.Minc -> Format.fprintf fmt "inc"
  | Isl.Mdec -> Format.fprintf fmt "dec"
  | Isl.Maddi n -> Format.fprintf fmt "add $%s" (Int64.to_string n)
  | Isl.Mimuli n -> Format.fprintf fmt "imul $%s" (Int64.to_string n)
  | Isl.Midivil n -> Format.fprintf fmt "idiv ($%s)" (Int64.to_string n)
  | Isl.Midivir n -> Format.fprintf fmt "idiv $%s" (Int64.to_string n)
  | Isl.Mmodil n -> Format.fprintf fmt "mod ($%s)" (Int64.to_string n)
  | Isl.Mmodir n -> Format.fprintf fmt "mod $%s" (Int64.to_string n)
  | Isl.Msetei n -> Format.fprintf fmt "sete %s" (Int64.to_string n)
  | Isl.Msetnei n -> Format.fprintf fmt "setne %s" (Int64.to_string n)
  | Isl.Msetgi n -> Format.fprintf fmt "setg %s" (Int64.to_string n)
  | Isl.Msetgei n -> Format.fprintf fmt "setge %s" (Int64.to_string n)
  | Isl.Msetli n -> Format.fprintf fmt "setl %s" (Int64.to_string n)
  | Isl.Msetlei n -> Format.fprintf fmt "setle %s" (Int64.to_string n)

let print_isl_binop fmt = function
  | Isl.Mmov -> Format.fprintf fmt "mov"
  | Isl.Madd -> Format.fprintf fmt "add"
  | Isl.Msub -> Format.fprintf fmt "sub"
  | Isl.Mxor -> Format.fprintf fmt "xor"
  | Isl.Mimul -> Format.fprintf fmt "imul"
  | Isl.Midiv -> Format.fprintf fmt "idiv"
  | Isl.Mmod -> Format.fprintf fmt "mod"
  | Isl.Msete -> Format.fprintf fmt "sete"
  | Isl.Msetne -> Format.fprintf fmt "setne"
  | Isl.Msetg -> Format.fprintf fmt "setg"
  | Isl.Msetge -> Format.fprintf fmt "setge"
  | Isl.Msetl -> Format.fprintf fmt "setl"
  | Isl.Msetle -> Format.fprintf fmt "setle"

let rec print_isl_expr fmt e =
  match e.desc with
  | Isl.IEint n ->
     Format.fprintf fmt "Eint %s" (Int64.to_string n)
  | Isl.IEstring s ->
     Format.fprintf fmt "Estring %s" s
  | Isl.IEbool b ->
     Format.fprintf fmt "Ebool %s" (if b then "true" else "false")
  | Isl.IEnil ->
     Format.fprintf fmt "Enil"
  | Isl.IElist el ->
     Format.fprintf fmt "Elist [%a]" print_isl_expr_l el
  | Isl.IEmalloc n ->
     Format.fprintf fmt "Emalloc %s" (Int32.to_string n)
  | Isl.IEaccess v ->
     Format.fprintf fmt "Eaccess %s" v
  | Isl.IEselect (s, n) ->
     Format.fprintf fmt "Eselect %d(%a)" n print_isl_expr s
  | Isl.IEload (s, n) ->
     Format.fprintf fmt "Eload %d(%a)" n print_isl_expr s
  | Isl.IEcall (f, actuals) ->
     Format.fprintf fmt "Ecall %s(%a)" f print_isl_expr_l actuals
  | Isl.IEunop (op, e) ->
     Format.fprintf fmt "Eunop %a (%a)" print_isl_munop op print_isl_expr e
  | Isl.IEaddr e ->
     Format.fprintf fmt "Eaddr (%a)" print_isl_expr e
  | Isl.IEbinop (op, e1, e2) ->
     Format.fprintf fmt "Ebinop %a (%a, %a)" print_isl_binop op print_isl_expr e1 print_isl_expr e2
  | Isl.IEand (e1, e2) ->
     Format.fprintf fmt "Eand (%a, %a)" print_isl_expr e1 print_isl_expr e2
  | Isl.IEor (e1, e2) ->
     Format.fprintf fmt "Eor (%a, %a)" print_isl_expr e1 print_isl_expr e2

and print_isl_expr_l fmt =
  print_list fmt ", " print_isl_expr

let print_isl_assign fmt instr =
  match instr.assignee with
  | Isl.Avar v -> Format.fprintf fmt "%s" v
  | Isl.Afield_var (v, n) -> Format.fprintf fmt "%s.%d" v n
  | Isl.Afield (s, n) -> Format.fprintf fmt "%a.%d" print_isl_expr s n
  | Isl.Adref (s, n) -> Format.fprintf fmt "%a->%d" print_isl_expr s n
     
let rec print_isl_stmt fmt = function
  | Isl.ISexpr e ->
     Format.fprintf fmt "\t%a" print_isl_expr e
  | Isl.IScall (f, actuals) ->
     Format.fprintf fmt "\tScall %s(%a)" f print_isl_expr_l actuals
  | Isl.ISprint args ->
     Format.fprintf fmt "\tSprint (%a)" print_isl_expr_l args
  | Isl.ISif (cond, bif, belse) ->
     Format.fprintf fmt "\tSif (%a) t{%a} e{%a}" print_isl_expr cond print_isl_block bif
       print_isl_block belse
  | Isl.ISassign (vars, values) ->
     let print_isl_assign_l fmt = print_list fmt ", " print_isl_assign in
     Format.fprintf fmt "\tSassign [%a = %a]" print_isl_assign_l vars print_isl_expr_l values
  | Isl.ISreturn es ->
     Format.fprintf fmt "\tSreturn [%a]" print_isl_expr_l es
  | Isl.ISfor (cond, b) ->
     Format.fprintf fmt "\tSfor (%a) b{%a}" print_isl_expr cond print_isl_block b

and print_isl_block fmt =
  print_list fmt "\n" print_isl_stmt

let print_isl_function fmt ((fname,f): string * Isl.ifundef) =
  Format.fprintf fmt "@[***** ISL: func %s *****@]@." fname;
  Format.fprintf fmt "@[%a@]@." print_isl_block f.body;
  Format.fprintf fmt "@[***** done *****@]@."
  
let isl_file (f:Isl.iprogramme) =
  Format.printf "%a" print_smap (print_isl_function,  f.functions)

(** {2 RTL} **)

let print_reg_list fmt =
  print_list fmt ", " Register.print

let print_rtl_mubranch fmt = function
  | Rtl.Mjz -> Format.fprintf fmt "jz"
  | Rtl.Mjnz -> Format.fprintf fmt "jnz"
  | Rtl.Mjei n -> Format.fprintf fmt "je $%s" (Int64.to_string n)
  | Rtl.Mjnei n -> Format.fprintf fmt "jnz $%s" (Int64.to_string n)
  | Rtl.Mjgi n -> Format.fprintf fmt "jg $%s" (Int64.to_string n)
  | Rtl.Mjgei n -> Format.fprintf fmt "jge $%s" (Int64.to_string n)
  | Rtl.Mjli n -> Format.fprintf fmt "jl $%s" (Int64.to_string n)
  | Rtl.Mjlei n -> Format.fprintf fmt "jle $%s" (Int64.to_string n)

let print_rtl_mbbranch fmt = function
  | Rtl.Mje -> Format.fprintf fmt "je"
  | Rtl.Mjne -> Format.fprintf fmt "jne"
  | Rtl.Mjg -> Format.fprintf fmt "jg"
  | Rtl.Mjge -> Format.fprintf fmt "jge"
  | Rtl.Mjl -> Format.fprintf fmt "jl"
  | Rtl.Mjle -> Format.fprintf fmt "jle"

let rec print_rtl_instr fmt (lab, instr) =
  match instr with
  | Rtl.Iint (n, r, l) ->
     Format.fprintf fmt "\t%a: mov $%s %a --> %a" Label.print lab
       (Int64.to_string n) Register.print r Label.print l
  | Rtl.Istring (s, r, l) ->
     Format.fprintf fmt "\t%a: mov \"%s\" %a --> %a" Label.print lab
       s Register.print r Label.print l
  | Rtl.Ibool (b, r, l) ->
     Format.fprintf fmt "\t%a: mov %s %a --> %a" Label.print lab
       (if b then "true" else "false") Register.print r Label.print l
  | Rtl.Imalloc (r, n, l) ->
     Format.fprintf fmt "\t%a: malloc %s -->%a" Label.print lab
       (Int32.to_string n) Label.print l
  | Rtl.Ilea_local (rxs, ofs, dst, l) ->
     Format.fprintf fmt "\t%a: lea_l %d(%a) %a --> %a" Label.print lab
       ofs print_reg_list rxs Register.print dst Label.print l
  | Rtl.Ilea (src, ofs, dst, l) ->
     Format.fprintf fmt "\t%a: lea %d(%a) %a --> %a" Label.print lab
       ofs Register.print src Register.print dst Label.print l
  | Rtl.Iload (src, ofs, dst, l) ->
     Format.fprintf fmt "\t%a: mov %d(%a) %a --> %a" Label.print lab
       ofs Register.print src Register.print dst Label.print l
  | Rtl.Istore (src, dst, ofs, l) ->
     Format.fprintf fmt "\t%a: mov %a %d(%a) --> %a" Label.print lab
       Register.print src ofs Register.print dst Label.print l
  | Rtl.Icall (res, f, actuals, l) ->
     Format.fprintf fmt "\t%a: %a := call %s(%a) --> %a" Label.print lab
       print_reg_list res f print_reg_list actuals Label.print l
  | Rtl.Iprint (args, l) ->
     Format.fprintf fmt "\t%a: call printf %a --> %a" Label.print lab
       print_reg_list args Label.print l
  | Rtl.Imunop (op, r, l) ->
     Format.fprintf fmt "\t%a: %a %a --> %a" Label.print lab
       print_isl_munop op Register.print r Label.print l
  | Rtl.Iinc_dec (IDinc, r, n, l) ->
     Format.fprintf fmt "\t%a: inc %d(%a) --> %a" Label.print lab n
       Register.print r Label.print l
  | Rtl.Iinc_dec (IDdec, r, n, l) ->
     Format.fprintf fmt "\t%a: dec %d(%a) --> %a" Label.print lab n
       Register.print r Label.print l
  | Rtl.Imbinop (op, src, dst, l) ->
     Format.fprintf fmt "\t%a: %a %a %a --> %a" Label.print lab
       print_isl_binop op Register.print src Register.print dst Label.print l
  | Rtl.Imubranch (op, r, lt, lf) ->
     Format.fprintf fmt "\t%a: %a %a --> %a, %a" Label.print lab
       print_rtl_mubranch op Register.print r Label.print lt Label.print lf
  | Rtl.Imbbranch (op, r1, r2, lt, lf) ->
     Format.fprintf fmt "\t%a: %a %a %a --> %a, %a" Label.print lab
       print_rtl_mbbranch op Register.print r1 Register.print r2
       Label.print lt Label.print lf
  | Rtl.Igoto l ->
     Format.fprintf fmt "\t%a: goto %a" Label.print lab Label.print l

let print_label_map fmt (print_pair, map) =
  Label.M.iter (fun l i -> Format.fprintf fmt "%a\n" print_pair (l, i)) map

let print_rtl_function fmt ((fname,f): string * Rtl.rfundef) =
  Format.fprintf fmt "@[***** RTL: func %s *****@]@." fname;
  Format.fprintf fmt "@[%a <== %s(%a)@]@." print_reg_list f.result fname
    print_reg_list f.formals;
  Format.fprintf fmt "@[entry: %a@]@." Label.print f.entry;
  Format.fprintf fmt "@[exit: %a@]@." Label.print f.exit_;
  Format.fprintf fmt "@[locals: %a@]@." print_reg_set f.locals;
  Format.fprintf fmt "@[body:@,%a@]@?" print_label_map (print_rtl_instr, f.body);
  Format.fprintf fmt "@[***** done *****@]@."

let rtl_file (f:Rtl.rprogramme) =
  Format.printf "%a" print_smap (print_rtl_function, f.functions)

(** {2 ERTL} **)

let print_ertl_munop fmt = function
  | Ertl.Mnot -> Format.fprintf fmt "not"
  | Ertl.Mneg -> Format.fprintf fmt "neg"
  | Ertl.Minc -> Format.fprintf fmt "inc"
  | Ertl.Mdec -> Format.fprintf fmt "dec"
  | Ertl.Maddi n -> Format.fprintf fmt "add $%s" (Int64.to_string n)
  | Ertl.Mimuli n -> Format.fprintf fmt "imul $%s" (Int64.to_string n)
  | Ertl.Msetei n -> Format.fprintf fmt "sete %s" (Int64.to_string n)
  | Ertl.Msetnei n -> Format.fprintf fmt "setne %s" (Int64.to_string n)
  | Ertl.Msetgi n -> Format.fprintf fmt "setg %s" (Int64.to_string n)
  | Ertl.Msetgei n -> Format.fprintf fmt "setge %s" (Int64.to_string n)
  | Ertl.Msetli n -> Format.fprintf fmt "setl %s" (Int64.to_string n)
  | Ertl.Msetlei n -> Format.fprintf fmt "setle %s" (Int64.to_string n)

let print_ertl_binop fmt = function
  | Ertl.Mmov -> Format.fprintf fmt "mov"
  | Ertl.Madd -> Format.fprintf fmt "add"
  | Ertl.Msub -> Format.fprintf fmt "sub"
  | Ertl.Mxor -> Format.fprintf fmt "xor"
  | Ertl.Mimul -> Format.fprintf fmt "imul"
  | Ertl.Msete -> Format.fprintf fmt "sete"
  | Ertl.Msetne -> Format.fprintf fmt "setne"
  | Ertl.Msetg -> Format.fprintf fmt "setg"
  | Ertl.Msetge -> Format.fprintf fmt "setge"
  | Ertl.Msetl -> Format.fprintf fmt "setl"
  | Ertl.Msetle -> Format.fprintf fmt "setle"

let print_ertl_instr fct fmt (lab, instr) =
  match instr with
  | Ertl.Iint (n, r, l) ->
     Format.fprintf fmt "\t%a: mov $%s %a --> %a\t%a" Label.print lab
       (Int64.to_string n) Register.print r Label.print l fct lab
  | Ertl.Istring (s, r, l) ->
     Format.fprintf fmt "\t%a: mov \"%s\" %a --> %a\t%a" Label.print lab s
       Register.print r Label.print l fct lab
  | Ertl.Ibool (b, r, l) ->
     Format.fprintf fmt "\t%a: mov b$%d %a --> %a\t%a" Label.print lab
       (if b then 1 else 0) Register.print r Label.print l fct lab
  | Ertl.Ilea_local (rx, ofs, dst, l) ->
     Format.fprintf fmt "\t%a: lea_l %d(%a) %a --> %a\t%a" Label.print lab ofs
       Register.print rx Register.print dst Label.print l fct lab
  | Ertl.Ilea (src, ofs, dst, l) ->
     Format.fprintf fmt "\t%a: lea %d(%a) %a --> %a\t%a" Label.print lab ofs
       Register.print src Register.print dst Label.print l fct lab
  | Ertl.Iload (src, ofs, dst, l) ->
     Format.fprintf fmt "\t%a: mov %d(%a) %a --> %a\t%a" Label.print lab ofs
       Register.print src Register.print dst Label.print l fct lab
  | Ertl.Istore (src, dst, ofs, l) ->
     Format.fprintf fmt "\t%a: mov %a %d(%a) --> %a\t%a" Label.print lab
       Register.print src ofs Register.print dst Label.print l fct lab
  | Ertl.Icall (f, regs, l) ->
     Format.fprintf fmt "\t%a: call %s --> %a\t%a" Label.print lab f
       Label.print l fct lab
  | Ertl.Imunop (op, r, l) ->
     Format.fprintf fmt "\t%a: %a %a --> %a\t%a" Label.print lab
       print_ertl_munop op Register.print r Label.print l fct lab
  | Ertl.Iidiv_imm (n, l) ->
     Format.fprintf fmt "\t%a: idiv %s --> %a\t%a" Label.print lab
       (Int64.to_string n) Label.print l fct lab
  | Ertl.Iidiv (r, l) ->
     Format.fprintf fmt "\t%a: idiv %a --> %a\t%a" Label.print lab
       Register.print r Label.print l fct lab
  | Ertl.Iinc_dec (IDinc, r, n, l) ->
     Format.fprintf fmt "\t%a: inc %d(%a) --> %a\t%a" Label.print lab n
       Register.print r Label.print l fct lab
  | Ertl.Iinc_dec (IDdec, r, n, l) ->
     Format.fprintf fmt "\t%a: dec %d(%a) --> %a\t%a" Label.print lab n
       Register.print r Label.print l fct lab
  | Ertl.Imbinop (op , src, dst, l) ->
     Format.fprintf fmt "\t%a: %a %a %a --> %a\t%a" Label.print lab
       print_ertl_binop op Register.print src Register.print dst
       Label.print l fct lab
  | Ertl.Imubranch (mj, r, lt, lf) ->
     Format.fprintf fmt "\t%a: %a %a --> %a, %a\t%a" Label.print lab
       print_rtl_mubranch mj Register.print r Label.print lt Label.print lf fct lab
  | Ertl.Imbbranch (mj, r1, r2, lt, lf) ->
     Format.fprintf fmt "\t%a: %a %a %a --> %a, %a\t%a" Label.print lab
       print_rtl_mbbranch mj Register.print r1 Register.print r2
       Label.print lt Label.print lf fct lab
  | Ertl.Igoto l ->
     Format.fprintf fmt "\t%a: goto %a\t%a" Label.print lab Label.print l fct lab
  | Ertl.Ialloc_frame l ->
     Format.fprintf fmt "\t%a: alloc_frame --> %a\t%a" Label.print lab
       Label.print l fct lab
  | Ertl.Ifree_frame l ->
     Format.fprintf fmt "\t%a: free_frame --> %a\t%a" Label.print lab
       Label.print l fct lab
  | Ertl.Ialloc_stack (n, l) ->
     Format.fprintf fmt "\t%a: alloc_stack %s --> %a\t%a" Label.print lab
       (Int32.to_string n) Label.print l fct lab
  | Ertl.Ifree_stack (n, l) ->
     Format.fprintf fmt "\t%a: free_stack %s --> %a\t%a" Label.print lab
       (Int32.to_string n) Label.print l fct lab
  | Ertl.Iget_param (n, r, l) ->
     Format.fprintf fmt "\t%a: get_param %d(%a) --> %a\t%a" Label.print lab n
       Register.print r Label.print l fct lab
  | Ertl.Iset_result (r, n, l) ->
     Format.fprintf fmt "\t%a: set_result %d(%a) --> %a\t%a" Label.print lab n
       Register.print r Label.print l fct lab
  | Ertl.Ipop_param (r, l) ->
     Format.fprintf fmt "\t%a: pop %a --> %a\t%a" Label.print lab
       Register.print r Label.print l fct lab
  | Ertl.Ipush_param (r, l) ->
     Format.fprintf fmt "\t%a: push %a --> %a\t%a" Label.print lab
       Register.print r Label.print l fct lab
  | Ertl.Ireturn ->
     Format.fprintf fmt "\t%a: return\t%a" Label.print lab fct lab

let print_liveness g fmt (lab, instr) =
  print_ertl_instr (fun fmt l ->
      let (info:Liveness.info) = Label.M.find l g in
      Format.fprintf fmt "in: %a@ out: %a" print_reg_set info.in_
        print_reg_set info.out_
    ) fmt (lab, instr)

let print_arcs fmt v (arcs:Interference.arcs) =
  Format.fprintf fmt "@[\t%a: prefs: {%a},@ intfs: {%a}@]@."
    Register.print v print_reg_set arcs.prefs print_reg_set arcs.intfs
  
let print_intf_graph fmt =
  Register.M.iter (print_arcs fmt)
   
let print_reg_and_colour fmt v = function
  | Colouring.Reg mr ->
     Format.fprintf fmt "@[\t%a -> reg %a@]@." Register.print v Register.print mr
  | Colouring.Spilt n ->
     Format.fprintf fmt "@[\t%a -> spilt %d@]@." Register.print v n
  | Colouring.Heap (s, h) ->
     Format.fprintf fmt "@[\t%a -> heap (s:%d, h:%d)@]@." Register.print v s h

let print_colouring fmt =
  Register.M.iter (print_reg_and_colour fmt)

let print_ertl_function fmt ((fname,f): string * Ertl.efundef) =
  Format.fprintf fmt "@[***** ERTL: func %s *****@]@," fname;
  Format.fprintf fmt "@[entry: %a@]@." Label.print f.entry;
  Format.fprintf fmt "@[locals: %a@]@.@." print_reg_set f.locals;
  let live = Liveness.perform_analysis f.body in
  Format.fprintf fmt "@[Liveness analysis:@,%a@]@." print_label_map (print_liveness live, f.body);
  let graph = Interference.build_graph live in
  Format.fprintf fmt "@[Interference graph:@,%a@]@." print_intf_graph graph;
  let colour, nlocals = Colouring.alloc_registers Register.allocable f.stored_locals graph in
  Format.fprintf fmt "@[Colouring:\n%a@]@?" print_colouring colour;
  Format.fprintf fmt "***** done *****"
  
let ertl_file (f:Ertl.eprogramme) =
  Format.printf "%a" print_smap (print_ertl_function, f)

(** {2 LTL} **)

let print_colour fmt = function
  | Colouring.Reg mr ->
     Register.print fmt mr
  | Colouring.Spilt n ->
     Format.fprintf fmt "%d(%a)" n Register.print Register.rbp
  | Colouring.Heap (s, h) ->
     Format.fprintf fmt "s:%d(%a), h:%d" s Register.print Register.rbp h
    
let print_ltl_instr fmt (lab, instr) =
  match instr with
  | Ltl.Iint (n, c, l) ->
     Format.fprintf fmt "\t%a: mov $%s %a --> %a" Label.print lab
       (Int64.to_string n) print_colour c Label.print l
  | Ltl.Istring (s, r, l) ->
     Format.fprintf fmt "\t%a: mov \"%s\" %a --> %a" Label.print lab s
       Register.print r Label.print l 
  | Ltl.Ibool (b, c, l) ->
     Format.fprintf fmt "\t%a: mov b$%d %a --> %a" Label.print lab
       (if b then 1 else 0) print_colour c Label.print l 
  | Ltl.Ilea (src, ofs, dst, l) ->
     Format.fprintf fmt "\t%a: lea %d(%a) %a --> %a" Label.print lab ofs
       Register.print src Register.print dst Label.print l 
  | Ltl.Iload (src, ofs, dst, l) ->
     Format.fprintf fmt "\t%a: mov %d(%a) %a --> %a" Label.print lab ofs
       Register.print src Register.print dst Label.print l 
  | Ltl.Istore (src, dst, ofs, l) ->
     Format.fprintf fmt "\t%a: mov %a %d(%a) --> %a" Label.print lab
       Register.print src ofs Register.print dst Label.print l 
  | Ltl.Icall (f, l) ->
     Format.fprintf fmt "\t%a: call %s --> %a" Label.print lab f Label.print l
  | Ltl.Imunop (op, c, l) ->
     Format.fprintf fmt "\t%a: %a %a --> %a" Label.print lab
       print_ertl_munop op print_colour c Label.print l 
  | Ltl.Iidiv_imm (n, l) ->
     Format.fprintf fmt "\t%a: idiv %s --> %a" Label.print lab
       (Int64.to_string n) Label.print l 
  | Ltl.Iidiv (c, l) ->
     Format.fprintf fmt "\t%a: idiv %a --> %a" Label.print lab
       print_colour c Label.print l
  | Ltl.Iinc_dec (IDinc, r, n, l) ->
     Format.fprintf fmt "\t%a: inc %d(%a) --> %a" Label.print lab n
       Register.print r Label.print l
  | Ltl.Iinc_dec (IDdec, r, n, l) ->
     Format.fprintf fmt "\t%a: dec %d(%a) --> %a" Label.print lab n
       Register.print r Label.print l
  | Ltl.Imbinop (op , c_src, c_dst, l) ->
     Format.fprintf fmt "\t%a: %a %a %a --> %a" Label.print lab
       print_ertl_binop op print_colour c_src print_colour c_dst Label.print l
  | Ltl.Imubranch (mj, c, lt, lf) ->
     Format.fprintf fmt "\t%a: %a %a --> %a, %a" Label.print lab
       print_rtl_mubranch mj print_colour c Label.print lt Label.print lf 
  | Ltl.Imbbranch (mj, c1, c2, lt, lf) ->
     Format.fprintf fmt "\t%a: %a %a %a --> %a, %a" Label.print lab
       print_rtl_mbbranch mj print_colour c1 print_colour c2
       Label.print lt Label.print lf
  | Ltl.Igoto l ->
     Format.fprintf fmt "\t%a: goto %a" Label.print lab Label.print l 
  | Ltl.Ipop (r, l) ->
     Format.fprintf fmt "\t%a: pop %a --> %a" Label.print lab
       Register.print r Label.print l 
  | Ltl.Ipush (c, l) ->
     Format.fprintf fmt "\t%a: push %a --> %a" Label.print lab
       print_colour c Label.print l 
  | Ltl.Ireturn ->
     Format.fprintf fmt "\t%a: return" Label.print lab

let print_ltl_function fmt ((fname,f): string * Ltl.lfundef) =
  Format.fprintf fmt "@[***** LTL: func %s *****@]@." fname;
  Format.fprintf fmt "@[entry: %a@]@." Label.print f.entry;
  Format.fprintf fmt "@[body:@,%a@]@?" print_label_map (print_ltl_instr, f.body);
  Format.fprintf fmt "@[***** done *****@]@."
    
let ltl_file (f:Ltl.lprogramme) =
  Format.printf "%a" print_smap (print_ltl_function, f)

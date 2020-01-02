
open Istree
open Rtltree
open Ertltree

let rec print_list fmt pp = function
  | [] ->
     ()
  | e :: l ->
     pp fmt e; print_list fmt pp l

let print_munop fmt = function
  | Mnot ->
     Format.fprintf fmt "not"
  | Mneg ->
     Format.fprintf fmt "neg"
  | Mdref ->
     Format.fprintf fmt "**"
  | Maddr ->
     Format.fprintf fmt "&"
  | Minc ->
     Format.fprintf fmt "inc"
  | Mdec ->
     Format.fprintf fmt "dec"
  | Mxor ->
     Format.fprintf fmt "xor"
  | Maddi n ->
     Format.fprintf fmt "add $%d" (Int32.to_int n)
  | Mimuli n ->
     Format.fprintf fmt "imul $%d" (Int32.to_int n)
  | Midivil n ->
     Format.fprintf fmt "idiv ($%d)" (Int32.to_int n)
  | Midivir n ->
     Format.fprintf fmt "idiv $%d" (Int32.to_int n)
  | Mmodil n ->
     Format.fprintf fmt "mod ($%d)" (Int32.to_int n)
  | Mmodir n ->
     Format.fprintf fmt "mod $%d" (Int32.to_int n)
  | Msetei n ->
     Format.fprintf fmt "sete %d" (Int32.to_int n)
  | Msetnei n ->
     Format.fprintf fmt "setne %d" (Int32.to_int n)
  | Msetgi n ->
     Format.fprintf fmt "setg %d" (Int32.to_int n)
  | Msetgei n ->
     Format.fprintf fmt "setge %d" (Int32.to_int n)
  | Msetli n ->
     Format.fprintf fmt "setl %d" (Int32.to_int n)
  | Msetlei n ->
     Format.fprintf fmt "setle %d" (Int32.to_int n)

let print_binop fmt = function
  | Mmov ->
     Format.fprintf fmt "mov"
  | Madd ->
     Format.fprintf fmt "add"
  | Msub ->
     Format.fprintf fmt "sub"
  | Mimul ->
     Format.fprintf fmt "imul"
  | Midiv ->
     Format.fprintf fmt "idiv"
  | Mmod ->
     Format.fprintf fmt "mod"
  | Msete ->
     Format.fprintf fmt "sete"
  | Msetne ->
     Format.fprintf fmt "setne"
  | Msetg ->
     Format.fprintf fmt "setg"
  | Msetge ->
     Format.fprintf fmt "setge"
  | Msetl ->
     Format.fprintf fmt "setl"
  | Msetle ->
     Format.fprintf fmt "setle"

let rec print_is_expr fmt = function
  | Istree.IEint n ->
     Format.fprintf fmt "\n\tEint %d" (Int32.to_int n)
  | Istree.IEstring s ->
     Format.fprintf fmt "\n\tEstring %s" s
  | Istree.IEbool b ->
     Format.fprintf fmt "\n\tEbool %s" (if b then "true" else "false")
  | Istree.IEnil ->
     Format.fprintf fmt "\n\tEnil"
  | Istree.IEmalloc n ->
     Format.fprintf fmt "\n\tEmalloc %d" n
  | Istree.IEaccess v ->
     Format.fprintf fmt "\n\tEaccess %s" v
  | Istree.IEload (s, n) ->
     Format.fprintf fmt "\n\tEload %d(%a)" n print_is_expr s
  | Istree.IEcall (f, actuals) ->
     Format.fprintf fmt "\n\tEcall %s(%a)" f print_is_list actuals
  | Istree.IEunop (op, e) ->
     Format.fprintf fmt "\n\tEunop %a %a" print_munop op print_is_expr e
  | Istree.IEbinop (op, e1, e2) ->
     Format.fprintf fmt "\n\tEbinop %a %a %a" print_binop op print_is_expr e1 print_is_expr e2
  | Istree.IEand (e1, e2) ->
     Format.fprintf fmt "\n\tEand %a %a" print_is_expr e1 print_is_expr e2
  | Istree.IEor (e1, e2) ->
     Format.fprintf fmt "\n\tEor %a %a" print_is_expr e1 print_is_expr e2

and print_is_list fmt =
  print_list fmt print_is_expr

let print_is_assign fmt = function
  | Istree.Avar v ->
     Format.fprintf fmt "%s" v
  | Istree.Afield (s, n) ->
     Format.fprintf fmt "%d(%a)" n print_is_expr s
  | Istree.Adref e ->
     Format.fprintf fmt "*%a" print_is_expr e
     
let rec print_is_stmt fmt = function
  | Istree.ISexpr e ->
     print_is_expr fmt e
  | Istree.IScall (f, actuals) ->
     Format.fprintf fmt "\n\tScall %s(%a)" f print_is_list actuals
  | Istree.ISprint (format, args) ->
     Format.fprintf fmt "\n\tSprint (%s, %a)" format print_is_list args
  | Istree.ISif (cond, bif, belse) ->
     Format.fprintf fmt "\n\tSif %a {%a} {%a}" print_is_expr cond print_is_block bif
       print_is_block belse
  | Istree.ISassign (vars, values) ->
     Format.fprintf fmt "\n\tSassign %a = %a" print_is_assign_l vars print_is_list values
  | Istree.ISreturn es ->
     Format.fprintf fmt "\n\tSreturn %a" print_is_list es
  | Istree.ISfor (cond, b) ->
     Format.fprintf fmt "\n\tSfor %a {%a}" print_is_expr cond print_is_block b

and print_is_assign_l fmt =
  print_list fmt print_is_assign
    
and print_is_block fmt =
  print_list fmt print_is_stmt

let print_mubranch fmt = function
  | Mjz ->
     Format.fprintf fmt "jz"
  | Mjnz ->
     Format.fprintf fmt "jnz"
  | Mjei n ->
     Format.fprintf fmt "je $%d" (Int32.to_int n)
  | Mjnei n ->
     Format.fprintf fmt "jnz $%d" (Int32.to_int n)
  | Mjgi n ->
     Format.fprintf fmt "jg $%d" (Int32.to_int n)
  | Mjgei n ->
     Format.fprintf fmt "jge $%d" (Int32.to_int n)
  | Mjli n ->
     Format.fprintf fmt "jl $%d" (Int32.to_int n)
  | Mjlei n ->
     Format.fprintf fmt "jle $%d" (Int32.to_int n)

let print_mbbranch fmt = function
  | Mje ->
     Format.fprintf fmt "je"
  | Mjne ->
     Format.fprintf fmt "jne"
  | Mjg ->
     Format.fprintf fmt "jg"
  | Mjge ->
     Format.fprintf fmt "jge"
  | Mjl ->
     Format.fprintf fmt "jl"
  | Mjle ->
     Format.fprintf fmt "jle"

let print_reg_list fmt =
  let pp fmt r = Format.fprintf fmt "%a" Register.string_of_reg r in
  print_list fmt pp

let rec print_rtl_instr fmt lab = function
  | Rtltree.Eint (n, r, l) ->
     Format.fprintf fmt "\n\t%a: mov $%d %a --> %a" Label.string_of_label lab
       (Int32.to_int n) Register.string_of_reg r
       Label.string_of_label l
  | Rtltree.Estring (s, r, l) ->
     Format.fprintf fmt "\n\t%a: mov %s %a --> %a" Label.string_of_label lab
       s Register.string_of_reg r Label.string_of_label l
  | Rtltree.Ebool (b, r, l) ->
     Format.fprintf fmt "\n\t%a: mov %s %a --> %a" Label.string_of_label lab
       (if b then "true" else "false")
       Register.string_of_reg r Label.string_of_label l
  | Rtltree.Emalloc (r, n, l) ->
     Format.fprintf fmt "\n\t%a: malloc -->%a" Label.string_of_label lab
       Label.string_of_label l
  | Rtltree.Elea (src, dst, l) ->
     Format.fprintf fmt "\n\t%a: lea %a %a --> %a" Label.string_of_label lab
       Register.string_of_reg src Register.string_of_reg dst
       Label.string_of_label l
  | Rtltree.Eload (src, ofs, dst, l) ->
     Format.fprintf fmt "\n\t%a: mov %d(%a) %a --> %a" Label.string_of_label lab
       ofs Register.string_of_reg src
       Register.string_of_reg dst Label.string_of_label l
  | Rtltree.Estore_field (src, dst, ofs, l) ->
     Format.fprintf fmt "\n\t%a: mov %a %d(%a) --> %a" Label.string_of_label lab
       Register.string_of_reg src ofs
       Register.string_of_reg dst Label.string_of_label l
  | Rtltree.Estore_dref (src, dst, l) ->
     Format.fprintf fmt "\n\t%a: mov (%a) (%a) --> %a" Label.string_of_label lab
       Register.string_of_reg src
       Register.string_of_reg dst Label.string_of_label l
  | Rtltree.Ecall (res, f, actuals, l) ->
     Format.fprintf fmt "\n\t%a: %a := call %s(%a) --> %a" Label.string_of_label lab
       print_reg_list res f print_reg_list actuals Label.string_of_label l
  | Rtltree.Eprint _ ->
     assert false
  | Rtltree.Emunop (op, r, l) ->
     Format.fprintf fmt "\n\t%a: %a %a --> %a" Label.string_of_label lab
       print_munop op Register.string_of_reg r
       Label.string_of_label l
  | Rtltree.Embinop (op, src, dst, l) ->
     Format.fprintf fmt "\n\t%a: %a %a %a --> %a" Label.string_of_label lab
       print_binop op Register.string_of_reg src
       Register.string_of_reg dst Label.string_of_label l
  | Rtltree.Emubranch (op, r, lt, lf) ->
     Format.fprintf fmt "\n\t%a: %a %a --> %a, %a" Label.string_of_label lab
       print_mubranch op Register.string_of_reg r
       Label.string_of_label lt Label.string_of_label lf
  | Rtltree.Embbranch (op, r1, r2, lt, lf) ->
     Format.fprintf fmt "\n\t%a: %a %a %a --> %a, %a" Label.string_of_label lab
       print_mbbranch op Register.string_of_reg r1
       Register.string_of_reg r2 Label.string_of_label lt Label.string_of_label lf
  | Rtltree.Egoto l ->
     Format.fprintf fmt "\n\t%a: goto %a" Label.string_of_label lab
       Label.string_of_label l

let print_ertl_instr fmt lab = function
  | Eint (n, r, l) ->
     Format.fprintf fmt "\n\t%a: mov $%d %a --> %a" Label.string_of_label lab
       (Int32.to_int n) Register.string_of_reg r
       Label.string_of_label l
  | Estring (s, r, l) ->
     Format.fprintf fmt "\n\t%a: mov %s %a --> %a" Label.string_of_label lab
       s Register.string_of_reg r
       Label.string_of_label l
  | Ebool (b, r, l) ->
     Format.fprintf fmt "\n\t%a: mov b$%d %a --> %a" Label.string_of_label lab
       (if b then 1 else 0) Register.string_of_reg r
       Label.string_of_label l
  | Elea (src, dst, l) ->
     Format.fprintf fmt "\n\t%a: lea %a %a --> %a" Label.string_of_label lab
       Register.string_of_reg src Register.string_of_reg dst
       Label.string_of_label l
  | Eload (src, ofs, dst, l) ->
     Format.fprintf fmt "\n\t%a: mov %d(%a) %a --> %a" Label.string_of_label lab
       ofs Register.string_of_reg src
       Register.string_of_reg dst Label.string_of_label l
  | Estore_field (src, dst, ofs, l) ->
     Format.fprintf fmt "\n\t%a: mov %a %d(%a) --> %a" Label.string_of_label lab
       Register.string_of_reg src ofs
       Register.string_of_reg dst Label.string_of_label l
  | Estore_dref (src, dst, l) ->
     Format.fprintf fmt "\n\t%a: mov (%a) %a --> %a" Label.string_of_label lab
       Register.string_of_reg src Register.string_of_reg dst
       Label.string_of_label l
  | Ecall (res, f, regs, l) ->
     Format.fprintf fmt "\n\t%a: call %s --> %a" Label.string_of_label lab
       f Label.string_of_label l
  | Emunop (op, r, l) ->
     Format.fprintf fmt "\n\t%a: %a %a --> %a" Label.string_of_label lab
       print_munop op Register.string_of_reg r
       Label.string_of_label l
  | Embinop (op , src, dst, l) ->
     Format.fprintf fmt "\n\t%a: %a %a %a --> %a" Label.string_of_label lab
       print_binop op Register.string_of_reg src
       Register.string_of_reg dst Label.string_of_label l
  | Emubranch (mj, src, lt, lf) ->
     Format.fprintf fmt "\n\t%a: %a %a --> %a, %a" Label.string_of_label lab
       print_mubranch mj Register.string_of_reg src
       Label.string_of_label lt Label.string_of_label lf
  | Embbranch (mj, src, dst, lt, lf) ->
     Format.fprintf fmt "\n\t%a: %a %a %a --> %a, %a" Label.string_of_label lab
       print_mbbranch mj Register.string_of_reg src
       Register.string_of_reg dst Label.string_of_label lt Label.string_of_label lf
  | Egoto l ->
     Format.fprintf fmt "\n\t%a: goto %a" Label.string_of_label lab
       Label.string_of_label l
  | Ealloc_frame l ->
     Format.fprintf fmt "\n\t%a: alloc_frame --> %a" Label.string_of_label lab
       Label.string_of_label l
  | Efree_frame l ->
     Format.fprintf fmt "\n\t%a: free_frame --> %a" Label.string_of_label lab
       Label.string_of_label l
  | Eget_param (n, r, l) ->
     Format.fprintf fmt "\n\t%a: get_param %d(%a) --> %a" Label.string_of_label lab
       n Register.string_of_reg r Label.string_of_label l
  | Epop_param (r, l) ->
     Format.fprintf fmt "\n\t%a: pop %a --> %a" Label.string_of_label lab
       Register.string_of_reg r Label.string_of_label l
  | Epush_param (r, l) ->
     Format.fprintf fmt "\n\t%a: push %a --> %a" Label.string_of_label lab
       Register.string_of_reg r Label.string_of_label l
  | Ereturn ->
     Format.fprintf fmt "\n\t%a: return" Label.string_of_label lab

let pp_label_M fmt pp_fct =
  Label.M.iter (pp_fct fmt)

let pp_label_M_map_ertl fmt =
  pp_label_M fmt print_ertl_instr

let pp_label_M_map_rtl fmt =
  pp_label_M fmt print_rtl_instr

let print_rtl_function fmt f (funct:Rtltree.decl_fun) =
  Format.fprintf fmt "@[%a <== %s(%a):" print_reg_list funct.result f print_reg_list funct.formals;
  Format.fprintf fmt "\n\tentry : %a" Label.string_of_label funct.entry;
  Format.fprintf fmt "\n\texit : %a" Label.string_of_label funct.exit_;
  Format.fprintf fmt "\n\tlocals: {%a}" Pretty_printer.pp_set funct.locals;
  Format.printf "%a@]@." pp_label_M_map_rtl funct.body
   
let print_ertl_function fmt f funct =
  Format.printf "**  === ERTL: f ===  **\n";
  Format.fprintf fmt "@[%s():" f;
  Format.fprintf fmt "\n\tentry : %a" Label.string_of_label funct.entry;
  Format.fprintf fmt "\n\tlocals: {%a}" Pretty_printer.pp_set funct.locals;
  Format.printf "%a@]@." pp_label_M_map_ertl funct.body;
  Format.printf "**  === ERTL done ===  **";
  let live = Liveness.perform_analysis funct.body in
  Format.fprintf fmt "\nLiveness done\n";
  let graph = Interference.build_graph live in
  Format.fprintf fmt "\nInterference graph done:\n%a" Pretty_printer.pp_g graph;
  let colour = Colouring.alloc_registers Register.allocable graph in
  Format.fprintf fmt "\nColouring done:\n%a\n\n" Pretty_printer.pp_c colour
   
let pp_asg_smap fmt pp_fct =
  Asg.Smap.iter (pp_fct fmt)
  
let pp_asg_smap_ertl fmt =
  pp_asg_smap fmt print_ertl_function
  
let pp_asg_smap_rtl fmt =
  pp_asg_smap fmt print_rtl_function
  
let ertl_file f =
  Format.printf "%a" pp_asg_smap_ertl f.functions

let rtl_file (f:Rtltree.file) =
  Format.printf "%a" pp_asg_smap_rtl f.functions

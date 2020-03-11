
open Format
   
open Ast
open Asg
open Lexing

exception Syntax_error of Ast.loc * string
exception Type_error of Ast.loc * string

let red = "\027[31m"
let yellow = "\027[33m"
let blue = "\027[34m"
let invert = "\027[7m"
let close = "\027[0m"
let level = ref 0
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos
let word_size = 8 (* x86_64: 64b *)
let max_int = Big_int_Z.power_int_positive_int 2 63

let syntax_error loc msg =
  raise (Syntax_error (loc, msg))

let type_error loc msg =
  raise (Type_error (loc, msg))

let incr_level () = incr level
let decr_level () = decr level

let position_of_loc (b, e) =
  let line = b.pos_lnum in
  let first_char = b.pos_cnum - b.pos_bol + 1 in
  let last_char = e.pos_cnum - e.pos_bol in
  line, first_char, last_char
  
let sub_list l start len =
  let rec loop acc i n = function
    | [] ->
       if n = 0 then List.rev acc
       else raise (Invalid_argument "Not enough elements in the list")
    | x :: l ->
       if n = 0 then List.rev acc
       else begin
           if i < start then loop acc (succ i) n l
           else loop (x :: acc) (succ i) (pred n) l
         end
  in
  loop [] 0 len l

let split_list l n =
  let rec loop acc i = function
    | l when i = 0 ->
       List.rev acc, l
    | [] ->
       raise (Invalid_argument "Not enough elements in the list")
    | x :: xs ->
       loop (x :: acc) (pred i) xs
  in
  loop [] n l

let flatten l =
  let rec loop acc = function
    | [], [] ->
       acc
    | x :: l, rem ->
       loop ([x] :: acc) (l, rem)
    | [], l :: rem ->
       loop acc (l, rem)
  in loop [] ([], l)

let sum_of_list =
  List.fold_left (+) 0
    
let check_package pkg func func_loc =
  match pkg.desc with
  | Eident id when id = "fmt" ->
	 if func <> "Print" then
       syntax_error func_loc
         (Format.sprintf "unexpected function %s%s%s%s%s, expecting Print"
            invert yellow func close close)
  | _ ->
     syntax_error pkg.loc "expected package fmt"

let overflow n =
  Big_int_Z.ge_big_int n max_int
  
let underflow =
  let min_int = Big_int_Z.minus_big_int max_int in
  fun n -> Big_int_Z.lt_big_int n min_int
         
let check_int_size n loc =
  if !level =  0 && (overflow n || underflow n) then
    syntax_error loc
      (Format.sprintf "%s%s%s%s%s does not fit in 64 bits"
		 invert yellow (Big_int_Z.string_of_big_int n) close close)
  
let check_int n_str loc =
  let n = Big_int_Z.big_int_of_string n_str in
  check_int_size n loc;
  if !level land 1 = 1 then Big_int_Z.minus_big_int n
  else n
  
let check_neg_int e =
  match e.desc with
  | Eunop (Uneg, arg) ->
     begin
	   match arg.desc with
	   | Ecst (Cint n) ->
          check_int_size n e.loc;
          { arg with loc = e.loc }
       | Eunop (Uneg, e) ->
          e
       | _ ->
          e
	 end
  | _ ->
     assert false

let format_mid_string left centre right =
  Format.sprintf "%s%s%s%s%s%s%s" left invert yellow centre close close right

let string_of_unop fmt = function
  | Unot -> Format.fprintf fmt "!"
  | Uneg -> Format.fprintf fmt "-"
  | Udref -> Format.fprintf fmt "*"
  | Uaddr -> Format.fprintf fmt "&"
													  
let string_of_binop fmt = function
  | Badd -> Format.fprintf fmt "+" | Bsub -> Format.fprintf fmt "-"
  | Bmul -> Format.fprintf fmt "*" | Bdiv -> Format.fprintf fmt "/"
  | Bmod -> Format.fprintf fmt "%%" | Beq -> Format.fprintf fmt "=="
  | Bneq -> Format.fprintf fmt "!=" | Blt -> Format.fprintf fmt "<"
  | Ble -> Format.fprintf fmt "<=" | Bgt -> Format.fprintf fmt ">"
  | Bge -> Format.fprintf fmt ">=" | Band -> Format.fprintf fmt "&&"
  | Bor -> Format.fprintf fmt "||"
									  
let string_of_constant fmt = function
  | Cint n -> Format.fprintf fmt "%s" (Big_int_Z.string_of_big_int n)
  | Cstring str -> Format.fprintf fmt "\"%s\"" str
  | Cbool b -> Format.fprintf fmt (if b then "true" else "false")
  | Cnil -> Format.fprintf fmt "nil"
			  
let rec string_of_expr fmt = function
  | Ecst cst ->
     Format.fprintf fmt "%a" string_of_constant cst
  | Eident id ->
     Format.fprintf fmt "%s" id
  | Eselect (exp, (field, _)) ->
     Format.fprintf fmt "%a.%s" string_of_expr exp.desc field
  | Ecall (f, args) ->
     Format.fprintf fmt "%s()" f 
  | Eprint _ ->
     Format.fprintf fmt "fmt.Print()"
  | Eunop (op, expr) ->
     Format.fprintf fmt "%a(%a)" string_of_unop op string_of_expr expr.desc
  | Ebinop (op, l, r) ->
     Format.fprintf fmt "@[(%a %a@ %a)@]"
       string_of_expr l.desc string_of_binop op string_of_expr r.desc

let rec string_of_type_list fmt tl =
  Format.fprintf fmt "(";
  List.iteri
    (fun i t -> Format.fprintf fmt "%s%a" (if i > 0 then "," else "") string_of_type t) tl;
  Format.fprintf fmt ")"
  
and string_of_type fmt = function
  | TTint ->
     Format.fprintf fmt "int"
  | TTbool ->
     Format.fprintf fmt "bool"
  | TTstring ->
     Format.fprintf fmt "str"
  | TTnil | TTuntyped ->
     assert false
  | TTunit ->
     Format.fprintf fmt "unit"
  | TTstruct s ->
     Format.fprintf fmt "%s" s
  | TTtuple tl ->
     string_of_type_list fmt tl
  | TTpointer t ->
     Format.fprintf fmt "*%a" string_of_type t

let rec string_of_texpr fmt te =
  match te.tdesc with
  | TEint n ->
     Format.fprintf fmt "%s" (Int64.to_string n)
  | TEstring str ->
     Format.fprintf fmt "%s" str
  | TEbool b ->
     Format.fprintf fmt "%s" (if b then "true" else "false")
  | TEnil ->
     Format.fprintf fmt "nil"
  | TEnew typ ->
     Format.fprintf fmt "new(%a)" string_of_type typ
  | TEident tvar ->
     Format.fprintf fmt "%s" tvar.id
  | TEselect (struct_, field) | TEselect_dref (struct_, field) ->
     Format.fprintf fmt "%a.%s" string_of_texpr struct_ field
  | TEcall (f, args) ->
     Format.fprintf fmt "%s()" f 
  | TEprint _ ->
     Format.fprintf fmt "fmt.Print()"
  | TEunop (op, texpr) ->
     Format.fprintf fmt "%a(%a)" string_of_unop op string_of_texpr texpr
  | TEbinop (op, l, r) ->
     Format.fprintf fmt "@[(%a %a@ %a)@]"
       string_of_texpr l string_of_binop op string_of_texpr r

let rec string_of_list fmt string_of_el = function
  | [] ->
     ()
  | [x] ->
     string_of_el fmt x
  | x :: xs ->
     let partial fmt = string_of_list fmt string_of_el in
     Format.fprintf fmt "%a %a" string_of_el x partial xs
    
let length_of_type = function
  | TTint | TTstring | TTbool | TTstruct _ | TTpointer _ ->
     1
  | TTunit ->
     0
  | TTtuple tl ->
     List.length tl
  | TTnil | TTuntyped ->
     assert false

let get_ident e =
  match e.desc with
  | Eident id ->
     id, e.loc
  | _ as exp ->
	 syntax_error e.loc
       (Format.asprintf "unexpected expression %s%s%a%s%s, expecting string"
		  invert yellow string_of_expr exp close close)

let binop_expected_type = function
  | Badd | Bsub | Bmul | Bdiv | Bmod | Blt | Ble | Bgt | Bge ->
     Some TTint
  | Beq | Bneq ->
     None
  | Band | Bor ->
     Some TTbool
  
let verify_operand_type op loc term = function
  | None, t | Some TTnil, (TTpointer _ as t)  | Some (TTpointer _ as t), TTnil ->
     t
  | Some TTnil, TTnil ->
     type_error loc
       (Format.asprintf "invalid operation: nil %a nil (operator %a not defined on nil)"
          string_of_binop op string_of_binop op)
  | Some exp_typ, typ when typ = exp_typ ->
     typ
  | Some exp_typ, typ ->
     type_error loc (Format.asprintf "cannot convert %a (type %a) to type %a"
                       string_of_expr term string_of_type typ string_of_type exp_typ)

let single_texpr_compatible_types ty_ref ty_act loc f_msg =
  match ty_ref, ty_act with
  | t_r, t_a when t_r = t_a ->
     ()
  | TTpointer _, TTnil ->
     ()
  | TTuntyped, (TTint | TTstring | TTbool | TTstruct _ | TTpointer _) ->
     ()
  | TTuntyped, TTnil ->
     type_error loc (Format.asprintf "use of untyped nil")        
  | TTuntyped, _ | _, (TTunit | TTuntyped | TTtuple _) ->
     assert false
  | e_f, t_a ->
     type_error loc (f_msg ())
    
let multi_texpr_compatible_types ty_ref (te_act:Asg.texpr) f_msg =
  match ty_ref, te_act.typ with
  | t_r, t_a when t_r = t_a ->
     te_act
  | TTpointer _, TTnil ->
     (* TTnil's "unification" *)
     { te_act with typ = ty_ref }
  | TTuntyped, (TTint | TTstring | TTbool | TTstruct _ | TTpointer _) ->
     (* useless for checking of function parameters *)
     te_act
  | _, TTunit ->
     type_error te_act.loc
       (Format.asprintf "%a used as value" string_of_texpr te_act)
  | _, TTtuple _ ->
     type_error te_act.loc
       (Format.asprintf "multiple-value %a in single-value context" string_of_texpr te_act)
  | TTuntyped, TTnil ->
     type_error te_act.loc (Format.asprintf "use of untyped nil")
  | t_r, t_a ->
     type_error te_act.loc
       (Format.asprintf "cannot use %a (type %a) as type %a in %s"
          string_of_texpr te_act string_of_type t_a string_of_type t_r f_msg)

let rec scan_texpr env use_queue expr =
  match expr.tdesc with
  | TEint _ | TEstring _ | TEbool _ | TEnil | TEnew _ ->
     env, use_queue
  | TEident tvar when tvar.id = "_" ->
     env, use_queue
  | TEident tvar ->
     begin
       try
         let occ, loc = Smap.find tvar.id env in
         Smap.add tvar.id (occ + 1, loc) env, use_queue
       with Not_found ->
         env, tvar.id :: use_queue
     end
  | TEselect (str, _) | TEselect_dref (str, _) ->
     scan_texpr env use_queue str
  | TEcall (_, actuals) ->
     scan_texpr_list env use_queue actuals
  | TEprint texps ->
     scan_texpr_list env use_queue texps
  | TEunop (_, texp) ->
     scan_texpr env use_queue texp
  | TEbinop (_, l, r) ->
     let env, use_queue = scan_texpr env use_queue l in
     scan_texpr env use_queue r
          
and scan_texpr_list env use_queue texpr_list =
  List.fold_left (fun (env, queue) exp -> scan_texpr env queue exp) (env, use_queue) texpr_list

let rec reduce_queue env new_queue = function
  | [] ->
     env, new_queue
  | id :: queue ->
     let env, added =
       try let occ, loc = Smap.find id env in
           Smap.add id (occ + 1, loc) env, true
       with Not_found ->
         env, false
     in
     reduce_queue env (if added then new_queue else id :: new_queue) queue
     
let rec scan_tstmt loc env use_queue exp_rtype = function
  | TSnop ->
     env, use_queue, false
  | TScall (_, actuals) ->
     let env, use_queue = scan_texpr_list env use_queue actuals in
     env, use_queue, false
  | TSprint (texps) ->
     let env, use_queue = scan_texpr_list env use_queue texps in
     env, use_queue, false
  | TSincr texp | TSdecr texp ->
     let env, use_queue = scan_texpr env use_queue texp in
     env, use_queue, false
  | TSblock b ->
     scan_block loc env use_queue exp_rtype b
  | TSif (cond, bif, belse) ->
     let env, use_queue = scan_texpr env use_queue cond in
     let env, use_queue, fin_if = scan_block loc env use_queue exp_rtype bif in
     let env, use_queue, fin_else = scan_block loc env use_queue exp_rtype belse in
     env, use_queue, fin_if && fin_else
  | TSassign (to_be_assigned, values) ->
     (* left variables do not count *)
     let used_texprs =
       List.filter (fun exp -> match exp.tdesc with | TEident _ -> false | _ -> true) to_be_assigned in
     let env, use_queue = scan_texpr_list env use_queue used_texprs in
     let env, use_queue = scan_texpr_list env use_queue values in
     env, use_queue, false
  | TSdeclare (vars, values) ->
     let env, use_queue = scan_texpr_list env use_queue values in
     List.fold_left
       (fun env v ->
         if v.id = "_" then env else Smap.add v.id (0, v.loc) env
       ) env vars, use_queue, false
  | TSreturn te_actuals ->
     let env, use_queue = scan_texpr_list env use_queue te_actuals in
     begin
       match te_actuals with
       | [te_act] ->
          let act_rtype =
            match te_act.typ with
            | TTtuple tl ->
               tl
            | _ as t ->
               [t]
          in
          let cmp_exp_act = compare (List.length exp_rtype) (List.length act_rtype) in
          if cmp_exp_act = 0 then begin
              List.iter2
                (fun exp act ->
                  single_texpr_compatible_types exp act te_act.loc
                    (fun _ ->
                      Format.asprintf "cannot use %a value as type %a in return argument"
                        string_of_type act string_of_type exp)
                ) exp_rtype act_rtype;
              let env, use_queue = scan_texpr env use_queue te_act in
              env, use_queue, true
            end else
            let msg = if cmp_exp_act > 0 then "not enough" else "too many" in
            type_error loc
              (Format.asprintf "%s arguments to return\n\t have %a\n\t want %a"
                 msg string_of_type_list act_rtype string_of_type_list exp_rtype)
       | _ ->
          let cmp_exp_act = compare (List.length exp_rtype) (List.length te_actuals) in
          if cmp_exp_act = 0 then begin
              let env, use_queue =
                List.fold_left2
                  (fun (env, queue) exp te_act ->
                    ignore (multi_texpr_compatible_types exp te_act "return argument");
                    scan_texpr env queue te_act
                  ) (env, use_queue) exp_rtype te_actuals in
              env, use_queue, true
            end else
            let msg = if cmp_exp_act > 0 then "not enough" else "too many" in
            type_error loc
              (Format.asprintf "%s arguments to return\n\t have %a\n\t want %a"
                 msg string_of_type_list (List.map (fun te -> te.typ) te_actuals)
                 string_of_type_list exp_rtype)
     end
  | TSfor (cond, body) ->
     let sure_entry = cond.tdesc = TEbool true in
     let env, use_queue = scan_texpr env use_queue cond in
     let env, use_queue, fin_for = scan_block loc env use_queue exp_rtype body in
     env, use_queue, sure_entry && fin_for

and scan_block loc env use_queue rtype block =
  let b_env, b_queue, final =
    List.fold_left (fun (env, use_queue, _) st -> scan_tstmt loc env use_queue rtype st)
      (Smap.empty, [], false) block.stmts in
  Smap.iter
    (fun id (occ, loc) -> if occ = 0 then
                            type_error loc (Format.sprintf "%s declared and not used" id)) b_env;
  let env, use_queue = reduce_queue env use_queue b_queue in
  env, use_queue, final

let check_fun_return fun_env =
  let check_one_function name { formals; rtype; body; loc } =
    let env = List.fold_left (fun env (id, _) -> Smap.add id (0, loc) env) Smap.empty formals in
    let block = match body with | Typed b -> b | Untyped _ -> assert false in
    let exp_rtype =
      match rtype with
      | TTunit ->
         []
      | TTtuple tl ->
         tl
      | _ as t ->
         [t]
    in
    let env, _, final = scan_block loc env [] exp_rtype block in
    if not final && exp_rtype <> [] then type_error loc "missing return at end of function"
  in
  Smap.iter check_one_function fun_env

let check_recursive_struct struct_env =
  let check_one_struct root ({ fields; loc } as str) =
    let rec dfs_scan curr_struct =
      List.iter
        (function
         | _, TTstruct curr when curr = root -> 
            type_error loc  (Format.asprintf "invalid recursive type %s" root)
         | _, TTstruct curr ->
            dfs_scan (Smap.find curr struct_env)
         | _ ->
            ()
        ) curr_struct.fields
    in dfs_scan str 
  in
  Smap.iter check_one_struct struct_env
   
exception Found_main
        
let check_fun_main functions =
  try
    List.iter (fun (name, loc, args, rtype, _) ->
        if name = "main" then
          if args = [] && rtype = [] then raise Found_main
          else type_error loc "func main must have no arguments and no return values"
      ) functions;
    type_error dummy_loc "function main is undeclared in the main package"
  with Found_main -> ()

let prefix n l =
  let rec loop n acc l =
    if n = 0 then List.rev acc
    else
      match l with
      | [] ->
         assert false
      | x :: l ->
         loop (n-1) (x :: acc) l
  in
  loop n [] l

let inv_ubranch = function
  | Rtltree.Mjz ->
     Rtltree.Mjnz
  | Rtltree.Mjnz ->
     Rtltree.Mjz
  | Rtltree.Mjei n ->
     Rtltree.Mjnei n
  | Rtltree.Mjnei n ->
     Rtltree.Mjei n
  | Rtltree.Mjgi n ->
     Rtltree.Mjlei n
  | Rtltree.Mjgei n ->
     Rtltree.Mjli n
  | Rtltree.Mjli n ->
     Rtltree.Mjgei n
  | Rtltree.Mjlei n ->
     Rtltree.Mjgi n

let inv_bbranch = function
  | Rtltree.Mje ->
     Rtltree.Mjne
  | Rtltree.Mjne ->
     Rtltree.Mje
  | Rtltree.Mjg ->
     Rtltree.Mjle
  | Rtltree.Mjge ->
     Rtltree.Mjl
  | Rtltree.Mjl ->
     Rtltree.Mjge
  | Rtltree.Mjle ->
     Rtltree.Mjg

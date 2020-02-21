
open Asg

(* Function return conventions:
 *** 0 -> unit
 *** 1 -> tau
 *** 2+ -> list of tau 
 *)

let struct_env = ref Smap.empty
let func_env = ref Smap.empty
let import_fmt = ref false
let used_fmt = ref false
(* used for block enumeration *)
let block_number = ref (-1)
(* contains the exact expressions to be printed according to a printf-based format*)
let print_exprs = ref [] 

let next_bnumber () =
  incr block_number; !block_number

let type_of_string loc = function
  | "int" ->
     TTint
  | "string" ->
     TTstring
  | "bool" ->
     TTbool
  | _ as str ->
     try let _ = Smap.find str !struct_env in
         TTstruct str
     with Not_found ->
       Utils.type_error loc (Format.asprintf "undefined: %s" str)

let rec type_of_expr e =
  match e.Ast.desc with 
  | Ast.Eident str ->
     type_of_string e.loc str
  | Ast.Eunop (Udref, e_t) ->
     let typ = type_of_expr e_t in
     TTpointer typ
  | _ ->
     Utils.type_error e.loc (Format.asprintf "%a is not a type" Utils.string_of_expr e.desc)
    
let rec type_of_ast_typ = function
  | Ast.Tbasic (str, loc) ->
     type_of_string loc str
  | Ast.Tpointer typ ->
     TTpointer (type_of_ast_typ typ)

let rec format_by_type fmt te =
  match te.typ with 
  | TTint ->
     print_exprs := te :: !print_exprs;
     Format.fprintf fmt "%%d"
  | TTstring ->
     print_exprs := te :: !print_exprs;
     Format.fprintf fmt "%%s"
  | TTbool ->
     print_exprs := te :: !print_exprs;
     Format.fprintf fmt "%%d"
  | TTstruct str ->
     let fields = (Smap.find str !struct_env).fields in
     let is_assignable, loc = te.is_assignable, te.loc in
     let field_to_texpr (fd, typ) = { tdesc = TEselect (te, fd); typ; is_assignable; loc }
     in
     Format.fprintf fmt "{%a}" format_by_type_list (List.map field_to_texpr fields)
  | TTpointer typ ->
     print_exprs := te :: !print_exprs;
     Format.fprintf fmt "%%p"
  | TTtuple tl -> (* impossible to print the results of a multiple-return function *)
     (* TODO: handle Print(f()) where f returns multiple values *)
     assert false
  | TTnil | TTunit | TTuntyped ->
     assert false

and format_by_type_list fmt = function
  | [] ->
     ()
  | [t] ->
     format_by_type fmt t
  | t :: tl ->
     Format.fprintf fmt "%a %a" format_by_type t format_by_type_list tl

let format_by_type_main fmt te =
  match te.typ with
  | TTpointer (TTstruct str) ->
     (* There is a special case for pointers to struct *)
     let fields = (Smap.find str !struct_env).fields in
     let is_assignable, loc = te.is_assignable, te.loc in
     let field_to_texpr (fd, typ) =
       { tdesc = TEselect_dref (te, fd); typ; is_assignable; loc }
     in
     Format.fprintf fmt "&{%a}" format_by_type_list (List.map field_to_texpr fields)
  | _ ->
     format_by_type fmt te
    
(* TODO: move this to ertl: print '->' fields iff not nil *)
(* unify the strings: union-find *)
let format_of_texpr fmt te =
  match te.tdesc with
  | TEint n ->
     Format.fprintf fmt "%s" (Int64.to_string n)
  | TEstring s ->
     Format.fprintf fmt "%s" s
  | TEbool b -> (* Go prints "true"/"false", but it is hard to do so from assembly *)
     Format.fprintf fmt "%d" (if b then 1 else 0)
  | TEnil -> (* Go prints <nil>, but it is hard to do so from assembly *)
     Format.fprintf fmt "0"
  | _ ->
     format_by_type_main fmt te
    
let type_of_unop exp op t_exp =
  match op, t_exp.typ with
  | Ast.Unot, TTbool ->
     TTbool, false
  | Ast.Uneg, TTint ->
     TTint, false
  | Ast.Udref, TTpointer t ->
     t, exp.Ast.desc <> Ast.Ecst Cnil
  | Ast.Uaddr, t ->
     if t_exp.is_assignable then TTpointer t, false
     else Utils.type_error
            exp.loc (Format.asprintf "cannot take the address of %a" Utils.string_of_expr exp.desc)
  | Udref, TTnil ->
     Utils.type_error exp.loc (Format.asprintf "invalid indirect of nil")
  | _, TTuntyped ->
     assert false
  | (Ast.Unot, (TTint | TTstring | TTnil | TTunit | TTstruct _ | TTtuple _ | TTpointer _))
  | (Ast.Uneg, (TTstring | TTbool | TTnil | TTunit | TTstruct _ |  TTtuple _ | TTpointer _))
  | (Ast.Udref, (TTint | TTstring | TTbool | TTunit | TTstruct _ |TTtuple _)) ->
     Utils.type_error exp.loc
       (Format.asprintf "invalid operation: %a %a"
          Utils.string_of_unop op Utils.string_of_type t_exp.typ)

let type_of_binop = function
  | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv | Ast.Bmod ->
     TTint
  | Ast.Beq | Ast.Bneq | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge | Ast.Band | Ast.Bor ->
     TTbool

let tdesc_of_select t_str fd =
  match t_str.typ, t_str.tdesc with
  | TTpointer (TTstruct _), _ ->
     TEselect_dref (t_str, fd)
  | TTstruct _, TEunop (Ast.Udref, t_str) ->
     (* I'd rather use C's "->" than C's "." *)
     TEselect_dref (t_str, fd)
  | TTstruct _, _ ->
     TEselect (t_str, fd)
  | _, _ ->
     assert false
  
let new_var id b_number ?(loc=Utils.dummy_loc) ty =
  { id; b_number; ty; loc }

let underscore ty =
  { id = "_"; b_number = 0; ty; loc = Utils.dummy_loc }

(* unique expression `_` *)
let under_texpr =
  { tdesc = TEident (underscore TTuntyped); typ = TTuntyped; is_assignable = true; loc = Utils.dummy_loc }

let rec type_expr env (e:Ast.expr) =
  let tdesc, typ, is_assignable = compute_type env e in
  { tdesc; typ; is_assignable; loc = e.loc }
  
(** Inputs: 
            variable environment
            expression (AST)
 ** Outputs: 
            typed expression (ASG)
            type
            is assignable?
 *)
and compute_type env e =
  match e.desc with
  | Ast.Ecst (Cint n) ->
     TEint (Int64.of_string (Big_int_Z.string_of_big_int n)), TTint, false
  | Ast.Ecst (Cstring s) ->
     TEstring s, TTstring, false
  | Ast.Ecst (Cbool b) ->
     TEbool b, TTbool, false
  | Ast.Ecst Cnil ->
     TEnil, TTnil, false
  | Ast.Eident v when v = "_" ->
     Utils.type_error e.loc "cannot use _ as value"
  | Ast.Eident v ->
     begin
       try
         let tvar = Smap.find v env in
         TEident tvar, tvar.ty, true
       with Not_found ->
         Utils.type_error e.loc (Format.sprintf "undefined %s" v)
     end
  | Ast.Eselect (str, (fd, fd_loc)) ->
     begin
       let t_str = type_expr env str in
       match t_str.typ with
       | TTstruct s | TTpointer (TTstruct s) as t ->
          let { fields; loc = _ } = Smap.find s !struct_env in
          let fd_type =
            try List.assoc fd fields
            with Not_found ->
              Utils.type_error fd_loc (Format.asprintf "%a.%s undefined (type %a has no field %s)"
                                         Utils.string_of_expr str.desc fd Utils.string_of_type t fd)
          in
          tdesc_of_select t_str fd, fd_type, t_str.is_assignable
       | TTint | TTstring | TTbool | TTunit | TTtuple _ | TTpointer _ as t ->
          Utils.type_error str.loc (Format.asprintf "%a.%s undefined (type %a has no field %s)"
                                Utils.string_of_expr str.desc fd Utils.string_of_type t fd)
       | TTnil ->
          Utils.type_error str.loc (Format.sprintf "nil pointer dereference")
       | TTuntyped ->
          assert false
     end
  | Ast.Ecall (f, actuals) ->
     begin
       (* check if f has not been overriden by a variable *)
       try
         let var = Smap.find f env in
         Utils.type_error e.loc
           (Format.asprintf "cannot call non-function %s (type %a)" f Utils.string_of_type var.ty)
       with Not_found ->
         (* check if f has been defined as a function *)
         try
           let { formals; rtype; body = _; loc = _ } = Smap.find f !func_env in
           let actuals = type_fun_params env f e.loc formals actuals in
           TEcall (f, actuals), rtype, false
         with Not_found ->
           (* check if f is a built-in function *)
           if f = "new" then
             let t_actuals = List.map type_of_expr actuals in
             match t_actuals with
             | [] ->
                Utils.type_error e.loc "missing argument to new"
             | [t] ->
                TEnew t, TTpointer t, false
             | t :: _ ->
                Utils.type_error e.loc (Format.asprintf "too many arguments to new(%a)"
                                    Utils.string_of_type t)
           else
             Utils.type_error e.loc (Format.sprintf "undefined %s" f)
     end
  | Ast.Eprint el -> (* Behaviour in Go: this outputs (int, exn) *)
     TEprint (type_expr_list env el), TTunit, false
  | Ast.Eunop (op, e) ->
     let t_e = type_expr env e in
     let typ, is_assignable = type_of_unop e op t_e in
     TEunop (op, t_e), typ, is_assignable
  | Ast.Ebinop (op, l, r) ->
     let t_l = type_expr env l in
     let exp_type = Utils.binop_expected_type op in
     let left_exp_type = Utils.verify_operand_type op l.loc l.desc (exp_type, t_l.typ) in
     let t_r = type_expr env r in
     let _ = Utils.verify_operand_type op r.loc r.desc (Some left_exp_type, t_r.typ) in
     TEbinop (op, t_l, t_r), type_of_binop op, false

and type_expr_list env = function
  | [] ->
     []
  | [p] ->
     [ type_expr env p ]
  | _ as pl ->
     List.map
       (fun p -> let t_p = type_expr env p in
                 match t_p.typ with
                 | TTunit ->
                    Utils.type_error p.loc
                      (Format.asprintf "%a used as value" Utils.string_of_expr p.desc)
                 | TTtuple tl ->
                    Utils.type_error p.loc
                      (Format.asprintf "multiple-value %a in single-value context"
                         Utils.string_of_expr p.desc)
                 | _ ->
                    t_p
       ) pl
    
(* Checks if the types of the actual params match the expected ones (formal params) *)
and type_fun_params env f loc formals actuals =
  let ty_formals = List.map snd formals in
  match actuals with
  | [act] ->
     let te_act = type_expr env act in
     let ty_actuals =
       match te_act.typ with
       | TTtuple l ->
          l
       | TTunit ->
          Utils.type_error act.loc (Format.asprintf "%a used as value" Utils.string_of_expr act.desc)
       | _ as t ->
          [t]
     in
     let cmp_act_form = compare (List.length ty_actuals) (List.length ty_formals) in
     if cmp_act_form = 0 then begin
         List.iter2
           (fun t_f t_a ->
             Utils.single_texpr_compatible_types t_f t_a act.loc
               (fun _ -> Format.asprintf "cannot use %a value as type %a in argument to %s"
                           Utils.string_of_type t_a Utils.string_of_type t_f f)
           ) ty_formals ty_actuals;
         match ty_actuals with
         | [t_act] when t_act = TTnil ->
            (* TTnil's "unification" *)
            [{ te_act with typ = List.hd ty_formals }]
         | _ ->
            [te_act]
       end else
       let msg = if cmp_act_form > 0 then "too many" else "not enough" in
       Utils.type_error loc
         (Format.asprintf "%s arguments in call to %s\n\t have %a\n\t want %a"
            msg f Utils.string_of_type_list ty_actuals Utils.string_of_type_list ty_formals)
  | _  ->
     let cmp_act_form = compare (List.length actuals) (List.length ty_formals) in
     if cmp_act_form = 0 then begin
         List.map2
           (fun ty_form act -> 
             let t_act = type_expr env act in
             Utils.multi_texpr_compatible_types ty_form t_act (Format.sprintf "argument to %s" f)
           ) ty_formals actuals 
       end else
       let msg = if cmp_act_form > 0 then "too many" else "not enough" in
       Utils.type_error loc
         (Format.asprintf "%s arguments in call to %s\n\t have %a\n\t want %a"
            msg f Utils.string_of_type_list (List.map (fun act -> (type_expr env act).typ) actuals)
            Utils.string_of_type_list ty_formals)

let type_assigned_values env loc to_be_assigned values =
  let l_assigned = List.length to_be_assigned in
  match values with
  | [value] ->
     let te_value = type_expr env value in
     let t_values =
       match te_value.typ with
       | TTtuple vs ->
          vs
       | TTunit ->
          Utils.type_error value.loc
            (Format.asprintf "%a used as value" Utils.string_of_expr value.desc)
       | _ as vs ->
          [vs]
     in
     let l_values = List.length t_values in
     if l_assigned = l_values then begin
         List.iter2
           (fun te_ass t_v ->
             let f_msg =
               if l_values = 1
               then fun _ ->
                    Format.asprintf "cannot use %a (type %a) as type %a in multiple assignment"
                      Utils.string_of_texpr te_value Utils.string_of_type t_v
                      Utils.string_of_type te_ass.typ
               else fun _ ->
                    Format.asprintf "cannot assign %a to %a (type %a) in multiple assignment"
                      Utils.string_of_type t_v Utils.string_of_texpr te_ass
                      Utils.string_of_type te_ass.typ
             in
             Utils.single_texpr_compatible_types te_ass.typ t_v te_value.loc f_msg
           ) to_be_assigned t_values;
         match t_values with
         | [t_val] when t_val = TTnil ->
            (* TTnil's "unification" *)
            [{ te_value with typ = (List.hd to_be_assigned).typ }]
         | _ ->
            [te_value]
       end else
       Utils.type_error loc
         (Format.asprintf "assignment mismatch: %d variable(s) but %a returns %d value(s)"
            l_assigned Utils.string_of_expr value.desc l_values)
  | _  ->
     let l_values = List.length values in
     if l_assigned = l_values then begin
         List.map2
           (fun te_ass val_ ->
             let te_val = type_expr env val_ in
             Utils.multi_texpr_compatible_types te_ass.typ te_val "assignment"
           ) to_be_assigned values 
       end else
       Utils.type_error loc
         (Format.asprintf "assignment mismatch: %d variable(s) but %d value(s)" l_assigned l_values)
     
let type_underscores to_be_assigned t_values =
  let t_types =
    match t_values with
    | [{ tdesc; typ = TTtuple tl; is_assignable; loc }] ->
       tl
    | _ as vs ->
       List.map (fun t_v -> t_v.typ) vs
  in
  List.map2 (fun t_e t ->
      match t_e.tdesc with
      | TEident tvar when tvar.id = "_" ->
         { t_e with tdesc = TEident { tvar with ty = t } }
      | _ ->
         t_e
    ) to_be_assigned t_types
     
let update_env env var_map = function
  | TSdeclare (var_list, values) ->
     List.fold_left
       (fun env var -> if var.id = "_" then env
                       else try Smap.add var.id (Smap.find var.id var_map) env
                            with Not_found -> assert false
       ) env var_list
  | _ ->
     env

let expr_placeholder ?(typ=TTuntyped) = fun _ ->
  { tdesc = TEnil; typ; is_assignable = false; loc = Utils.dummy_loc }
     
let rec type_shstmt env b_vars b_number = function
  | Ast.Ieval e ->
     begin
       match e.desc with
       | Ecst _ | Eident _ | Eselect _ | Eunop _ | Ebinop _ ->
          Utils.type_error e.loc (Format.asprintf "%a evaluated but not used"
                                    Utils.string_of_expr e.desc)
       | Ecall (f, actuals) ->
          let t_call = type_expr env e in
          let actuals = match t_call.tdesc with | TEcall (_, act) -> act | _ -> assert false in
          b_vars, TScall (f, actuals)
       | Eprint el ->
          if not !import_fmt then
            Utils.type_error e.loc "undefined: fmt";
          used_fmt := true;
          let exprs = type_expr_list env el in
          let rec printer fmt = function
            | [] ->
               ()
            | [x] ->
               format_of_texpr fmt x
            | x :: xs ->
               Format.fprintf fmt "%a %a" format_of_texpr x printer xs
          in
          let format = Format.asprintf "%a" printer exprs in
          let p_exprs = List.rev !print_exprs in
          print_exprs := [];
          b_vars, TSprint (format, p_exprs)
     end
  | Ast.Iincr e | Idecr e as i_d->
     let string_of_op = match i_d with | Iincr _ -> "++" | Idecr _ -> "--" | _ -> assert false in
     let t_e = type_expr env e in
     if t_e.is_assignable then
       if t_e.typ = TTint
       then b_vars, match i_d with | Iincr _ -> TSincr t_e | Idecr _ -> TSdecr t_e | _ -> assert false
       else Utils.type_error e.loc
              (Format.asprintf "invalid operation %a%s (non-numeric type %a)"
                 Utils.string_of_expr e.desc string_of_op Utils.string_of_type t_e.typ)
     else Utils.type_error e.loc (Format.asprintf "cannot assign to %a" Utils.string_of_expr e.desc)
     
  | Ast.Iassign (assigned_s, values) ->
     let t_assigned_s = List.map
                          (fun ass -> if ass.Ast.desc = Ast.Eident "_" then under_texpr
                                      else
                                        let t_ass = type_expr env ass in
                                        if not t_ass.is_assignable then
                                          Utils.type_error ass.loc
                                            (Format.asprintf "cannot assign to %a"
                                               Utils.string_of_expr ass.desc);
                                        t_ass
                          ) assigned_s in
     let loc = try (List.hd values).loc with Failure _ -> assert false in
     let t_values = type_assigned_values env loc t_assigned_s values in
     let t_assigned_s = type_underscores t_assigned_s t_values in
     b_vars, TSassign (t_assigned_s, t_values)
  | Ast.Ideclare (vars, values) ->
     (* Behaviour in Go: this can also assign values provided at least one left var is new *)
     (* the 'local' environment is updated by the declaration statement *)
     let untyped_vars = List.map expr_placeholder vars in
     let loc = try (List.hd values).loc with Failure _ -> assert false in
     let t_values = type_assigned_values env loc untyped_vars values in
     let types =
       match t_values with
       | [{ tdesc; typ = TTtuple types; is_assignable; loc }] ->
          types
       | [{ tdesc; typ = (TTint|TTstring|TTbool|TTstruct _|TTpointer _ as t); is_assignable; loc }] ->
          [t]
       | [] ->
          assert false
       | _ as types ->
          List.map (fun t_e -> t_e.typ) types
     in
     let rec update_var_map (var_map:Asg.tvar Asg.Smap.t) t_vars = function
       | [], [] ->
          var_map, List.rev t_vars
       | [], _ | _, [] ->
          assert false
       | (id, loc) :: var_names, typ :: types when id = "_" ->
          (* variable "_" is not added to the local environment *)
          update_var_map var_map (underscore typ :: t_vars) (var_names, types)
       | (id, loc) :: var_names, typ :: types ->
          (* check name unicity *)
          try
            let previous_decl = Smap.find id var_map in
            let line, fst_char, last_char = Utils.position_of_loc previous_decl.loc in
            Utils.type_error loc
              (Format.sprintf
                 "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
                 id fst_char last_char line)
          with Not_found ->
            let n_var = new_var id b_number ~loc typ in
            update_var_map (Smap.add id n_var var_map) (n_var :: t_vars) (var_names, types)
     in
     let b_vars, t_assigned_s = update_var_map b_vars [] (vars, types) in
     b_vars, TSdeclare (t_assigned_s, t_values)

let rec type_stmt env b_vars b_number = function
  | Ast.Snop ->
     b_vars, TSnop
  | Ast.Sexec shstmt ->
     type_shstmt env b_vars b_number shstmt
  | Ast.Sblock b ->
     let number = next_bnumber () in
     let vars, stmts = type_block env b number in 
     b_vars, TSblock { vars; stmts; number }
  | Ast.Sif (cond, bif, belse) ->
     (* condition *)
     let t_cond = type_expr env cond in
     if t_cond.typ <> TTbool then
       Utils.type_error cond.loc
         (Format.asprintf "non-bool %a (type %a) used as if condition"
            Utils.string_of_expr cond.desc Utils.string_of_type t_cond.typ);
     (* branch then *)
     let if_number = next_bnumber () in
     let if_vars, if_stmts = type_block env bif if_number in
     (* branch else *)
     let else_number = next_bnumber () in
     let else_vars, else_stmts = type_block env belse else_number in
     b_vars, TSif (t_cond, { vars = if_vars; stmts = if_stmts; number = if_number },
                   { vars = else_vars; stmts = else_stmts; number = else_number })
  | Ast.Sinit (vars, ty, values) ->
     begin
       match values with
       | [] -> (* declaration *)
          (* syntax error if unspecified *)
          let typ =
            match ty with | Some t -> type_of_ast_typ t | None -> assert false in
          let rec update_var_map (var_map:Asg.tvar Asg.Smap.t) t_vars = function
            | [] ->
               var_map, List.rev t_vars
            | (id, loc) :: var_names when id = "_" ->
               (* variable "_" is not added to the local environment *)
               update_var_map var_map (underscore typ :: t_vars) var_names
            | (id, loc) :: var_names ->
               (* check name unicity *)
               try
                 let previous_decl = Smap.find id var_map in
                 let line, fst_char, last_char = Utils.position_of_loc previous_decl.loc in
                 Utils.type_error loc
                   (Format.sprintf
                      "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
                      id fst_char last_char line)
               with Not_found ->
                 let n_var = new_var id b_number ~loc typ in
                 update_var_map (Smap.add id n_var var_map) (n_var :: t_vars) var_names
                 
          in
          let b_vars, t_assigned_s = update_var_map b_vars [] vars in
          b_vars, TSdeclare (t_assigned_s, [])
       | _ -> (* declare-and-assign *)
          let typ =
            match ty with | Some t -> Some (type_of_ast_typ t) | None -> None in
          let vars_ph =
            match typ with
            | Some typ ->
               List.map (expr_placeholder ~typ) vars
            | None ->
               List.map expr_placeholder vars
          in
          let loc = (List.hd values).loc in
          let t_values = type_assigned_values env loc vars_ph values in
          let types =
            match t_values with
            | [{ tdesc; typ = TTtuple types; is_assignable; loc }] ->
               types
            | _ as types ->
               List.map (fun t_e -> t_e.typ) types
          in
          let rec update_var_map (var_map:Asg.tvar Asg.Smap.t) t_vars = function
            | [], [] ->
               var_map, List.rev t_vars
            | [], _ | _, [] ->
               assert false
            | (id, loc) :: var_names, typ :: types when id = "_" ->
               (* variable "_" is not added to the local environment *)
               update_var_map var_map (underscore typ :: t_vars) (var_names, types)
            | (id, loc) :: var_names, typ :: types ->
               (* check name unicity *)
               try
                 let previous_decl = Smap.find id var_map in
                 let line, fst_char, last_char = Utils.position_of_loc previous_decl.loc in
                 Utils.type_error loc
                   (Format.sprintf
                      "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
                      id fst_char last_char line)
               with Not_found ->
                 let n_var = new_var id b_number ~loc typ in
                 update_var_map (Smap.add id n_var var_map) (n_var :: t_vars) (var_names, types)
          in
          let b_vars, t_assigned_s = update_var_map b_vars [] (vars, types) in
          b_vars, TSdeclare (t_assigned_s, t_values)  
     end
  | Ast.Sreturn (exps) ->
     b_vars, TSreturn (type_expr_list env exps)
  | Ast.Sfor (init, cond, post, body) ->
     begin
       (* init statement*)
       let outer_vars, t_init, env, outer_number =
         match init with
         | None ->
            Smap.empty, TSnop, env, b_number
         | Some st ->
            let outer_number = next_bnumber () in
            let out_vars, t_init = type_shstmt env Smap.empty outer_number st in
            out_vars, t_init, update_env env out_vars t_init, outer_number
       in
       (* condition *)
       let t_cond = type_expr env cond in
       if t_cond.typ <> TTbool then
         Utils.type_error cond.loc
           (Format.asprintf "non-bool %a (type %a) used as for condition"
              Utils.string_of_expr cond.desc Utils.string_of_type t_cond.typ);
       (* post statement & body *)
       let for_number = next_bnumber () in
       let for_vars, for_stmts =
         match post with
         | None ->
            type_block env body for_number
         | Some st ->
            type_block env body ~ending_stmt:(Ast.Sexec st) for_number
       in
       let for_block = TSfor (t_cond, { vars = for_vars; stmts = for_stmts; number = for_number }) in
       b_vars,
       if outer_number = b_number then for_block
       else TSblock { vars = outer_vars; stmts = t_init :: [for_block]; number = outer_number }
     end
    
and type_block env stmts ?ending_stmt number =
  let rec loop env var_map stmts ending_stmt t_stmts number =
    match stmts, ending_stmt with
    | [], None ->
       var_map, List.rev t_stmts
    | [], Some st ->
       loop env var_map [st] None t_stmts number
    | st :: stmts, _ ->
       let var_map, t_st = type_stmt env var_map number st in
       (* the 'global' environment is updated by the block *)
       let env = update_env env var_map t_st in
       loop env var_map stmts ending_stmt (t_st :: t_stmts) number
  in loop env Smap.empty stmts ending_stmt [] number

let rec type_vars_of_decl (vars, typ) typ_list msg =
  match vars with
  | [] ->
     typ_list
  | (id, loc) :: vars ->
     try
       let _ = List.assoc id typ_list in
       Utils.type_error loc (Format.asprintf "duplicate %s %s" msg id)
     with Not_found ->
       type_vars_of_decl (vars, typ) ((id, typ) :: typ_list) msg

let type_vars_list msg vars =
  List.rev (
      List.fold_left
        (fun typ_list (vars, typ) ->
          let typ = type_of_ast_typ typ in
          type_vars_of_decl (vars, typ) typ_list msg
        ) [] vars
    )
   
let rec identify_declarations structs functions = function
  | [] ->
     List.rev structs, List.rev functions
  | Ast.Dstruct ((name, loc), fields) :: decls ->
     begin
       try let { fields = _; loc = previous_loc } = Smap.find name !struct_env in
           let line, fst_char, last_char = Utils.position_of_loc previous_loc in
           Utils.type_error loc
             (Format.asprintf
                "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
                name fst_char last_char line)
       with Not_found ->
         struct_env := Smap.add name { fields = []; loc } !struct_env
     end;
     identify_declarations ((name, loc, fields) :: structs) functions decls
    | Ast.Dfunc ((name, loc), args, rtype, body) :: decls ->
       identify_declarations structs ((name, loc, args, rtype, body) :: functions) decls

let type_fun_sign (name, loc, args, rtype, body) =
  try
    let { formals = _; rtype = _; body = _; loc = previous_loc } = Smap.find name !func_env in
    let line, fst_char, last_char = Utils.position_of_loc previous_loc in
    Utils.type_error loc
      (Format.asprintf
         "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
         name fst_char last_char line)
  with Not_found ->
    (* check arguments *)
    let formals = type_vars_list "argument" args in
    (* check return type *)
    let rtype =
      match List.map type_of_ast_typ rtype with
      | [] ->
         TTunit
      | [t] ->
         t
      | _ as ts ->
         TTtuple ts
    in
    func_env := Smap.add name { formals; rtype; body = Untyped body; loc } !func_env

let type_struct_fields (name, loc, fields) =
  (* type fields *)
  let fields = type_vars_list "field" fields in
  struct_env := Smap.add name { fields; loc } !struct_env

let type_fun_body (name, _, _, _, _) =
  let func = Smap.find name !func_env in
  block_number := 0;
  let b_number = 0 in
  let env = List.fold_left
              (fun env (id, t) -> Smap.add id (new_var id b_number t) env) Smap.empty func.formals in
  let body = match func.body with Untyped b -> b | Typed _ -> assert false in
  let vars, stmts = type_block env body b_number in
  (* check return *)
  let tbody = Typed { vars; stmts; number = b_number } in
  func_env := Smap.add name { func with body = tbody } !func_env

let programme file =
  import_fmt := fst file.Ast.import;
  let import_loc = snd file.Ast.import in
  let structs, functions = identify_declarations [] [] file.Ast.decls in
  List.iter type_fun_sign functions;
  List.iter type_struct_fields structs;
  List.iter type_fun_body functions;
  Utils.check_fun_return !func_env;
  Utils.check_recursive_struct !struct_env;
  Utils.check_fun_main functions;
  if !import_fmt <> !used_fmt then
    Utils.type_error import_loc "imported and not used: \"fmt\"";
  { structs = !struct_env; functions = !func_env }            

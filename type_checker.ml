
open Ty_ast

(* Convention of function return type:
 *** 0 -> unit
 *** 1 -> tau
 *** 2+ -> list of tau 
*)
   
exception Type_error of Ast.loc * string

let struct_env : Ty_ast.struct_ Smap.t ref = ref Smap.empty
let func_env : Ty_ast.func Smap.t ref = ref Smap.empty
let import_fmt = ref false
let used_fmt = ref false

let type_error loc msg =
  raise (Type_error (loc, msg))

let dummy_loc =
  Lexing.dummy_pos, Lexing.dummy_pos
  
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
       type_error loc (Format.asprintf "undefined: %s" str)

let rec type_of_expr e =
  match e.Ast.desc with 
  | Ast.Eident str ->
     type_of_string e.loc str
  | Ast.Eunop (Udref, e_t) ->
     let typ = type_of_expr e_t in
     TTpointer typ
  | _ ->
     type_error e.loc (Format.asprintf "%a is not a type" Utils.string_of_expr e.desc)
    
let rec type_of_ast_typ = function
  | Ast.Tbasic (str, loc) ->
     type_of_string loc str
  | Ast.Tpointer typ ->
     TTpointer (type_of_ast_typ typ)

let type_of_unop exp op t_exp =
  match op, t_exp.typ with
  | Ast.Unot, TTbool ->
     TTbool, false
  | Ast.Uneg, TTint ->
     TTint, false
  | Ast.Udref, TTpointer t ->
     t, exp.Ast.desc <> Ast.Ecst Ast.Cnil
  | Ast.Uaddr, t ->
     if t_exp.is_assignable then TTpointer t, false
     else type_error
            exp.loc (Format.asprintf "cannot take the address of %a" Utils.string_of_expr exp.desc)
  | Ast.Udref, TTnil ->
     type_error exp.loc (Format.asprintf "invalid indirect of nil")
  | _, TTuntyped ->
     assert false
  | (Ast.Unot, (TTint | TTstring | TTnil | TTunit | TTstruct _ | TTtuple _ | TTpointer _))
  | (Ast.Uneg, (TTstring | TTbool | TTnil | TTunit | TTstruct _ |  TTtuple _ | TTpointer _))
  | (Ast.Udref, (TTint | TTstring | TTbool | TTunit | TTstruct _ |TTtuple _)) ->
     type_error exp.loc
       (Format.asprintf "invalid operation: %a %a" Utils.string_of_unop op
          Utils.string_of_type t_exp.typ)

let binop_expected_type = function
  | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv | Ast.Bmod | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge ->
     Some TTint
  | Ast.Beq | Ast.Bneq ->
     None
  | Ast.Band | Ast.Bor ->
     Some TTbool
  
let type_of_binop = function
  | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv | Ast.Bmod ->
     TTint
  | Ast.Beq | Ast.Bneq | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge | Ast.Band | Ast.Bor ->
     TTbool
  
let verify_operand_type op loc term = function
  | None, t | Some TTnil, (TTpointer _ as t)  | Some (TTpointer _ as t), TTnil ->
     t
  | Some TTnil, TTnil ->
     type_error loc
       (Format.asprintf "invalid operation: nil %a nil (operator %a not defined on nil)"
          Utils.string_of_binop op Utils.string_of_binop op)
  | Some exp_typ, typ when typ = exp_typ ->
     typ
  | Some exp_typ, typ ->
     type_error loc (Format.asprintf "cannot convert %a (type %a) to type %a"
                       Utils.string_of_expr term Utils.string_of_type typ
                       Utils.string_of_type exp_typ)
    
let new_var id level ?(offset=0) ?(loc=dummy_loc) typ =
  { id; level; offset; typ; loc }

let underscore =
  { id = "_"; level = 0; offset = 0; typ = TTuntyped; loc = dummy_loc }

let under_texpr =
  { tdesc = TEident "_"; typ = TTuntyped; is_assignable = true; loc = dummy_loc }
  
let rec type_expr (env:Ty_ast.tvar Ty_ast.Smap.t) (e:Ast.expr) =
  let tdesc, typ, is_assignable = compute_type env e in
  { tdesc; typ; is_assignable; loc = e.loc }
  
(** Inputs: 
            variable environment
            expression (AST)
 ** Outputs: 
            typed expression (TT)
            type
            is assignable?
 *)
and compute_type env e =
  match e.desc with
  | Ast.Ecst (Cint n) ->
     TEint n, TTint, false
  | Ast.Ecst (Cstring s) ->
     TEstring s, TTstring, false
  | Ast.Ecst (Cbool b) ->
     TEbool b, TTbool, false
  | Ast.Ecst Cnil ->
     TEnil, TTnil, false
  | Ast.Eident v when v = "_" ->
     type_error e.loc "cannot use _ as value"
  | Ast.Eident v ->
     begin
       try
         TEident v, (let feats = Smap.find v env in feats.typ), true
       with Not_found ->
         type_error e.loc (Format.sprintf "undefined %s" v)
     end
  | Ast.Eselect (str, (fd, fd_loc)) ->
     begin
       let t_str = type_expr env str in
       match t_str.typ with
       | TTstruct s | TTpointer (TTstruct s) as t ->
          begin
            try
              let fields, _ = Smap.find s !struct_env in
              TEselect (t_str, fd), Smap.find fd fields, t_str.is_assignable
            with Not_found ->
              type_error fd_loc (Format.asprintf "%a.%s undefined (type %a has no field %s)"
                                   Utils.string_of_expr str.desc fd Utils.string_of_type t fd)
          end
       | TTint | TTstring | TTbool | TTunit | TTtuple _ | TTpointer _ as t ->
          type_error str.loc (Format.asprintf "%a.%s undefined (type %a has no field %s)"
                                Utils.string_of_expr str.desc fd Utils.string_of_type t fd)
       | TTnil ->
          type_error str.loc (Format.sprintf "nil pointer dereference")
       | TTuntyped ->
          assert false
     end
  | Ast.Ecall (f, actuals) ->
     begin
       (* check if f has not been overriden by a variable *)
       try
         let var = Smap.find f env in
         type_error e.loc
           (Format.asprintf "cannot call non-function %s (type %a)" f Utils.string_of_type var.typ)
       with Not_found ->
         (* check if f has been defined as a function *)
         try
           let formals, rtype, _, _ = Smap.find f !func_env in
           let actuals = check_fun_params env f e.loc formals actuals in
           TEcall (f, actuals), rtype, false
         with Not_found ->
           (* check if f is a built-in function *)
           if f = "new" then
             let t_actuals = List.map (type_of_expr) actuals in
             match t_actuals with
             | [] ->
                type_error e.loc "missing argument to new"
             | [t] ->
                TEnew t, TTpointer t, false
             | t :: _ ->
                type_error e.loc (Format.asprintf "too many arguments to new(%a)"
                                    Utils.string_of_type t)
           else
             type_error e.loc (Format.sprintf "undefined %s" f)
     end
  | Ast.Eprint el -> (* Behaviour in Go: this outputs (int, exn) *)
     if not !import_fmt then
       type_error e.loc "undefined: fmt";
     used_fmt := true;
     TEprint (check_expr_list env el), TTunit, false
  | Ast.Eunop (op, e) ->
     let t_e = type_expr env e in
     let typ, is_assignable = type_of_unop e op t_e in
     TEunop (op, t_e), typ, is_assignable
  | Ast.Ebinop (op, l, r) ->
     let t_l = type_expr env l in
     let exp_type = verify_operand_type op l.loc l.desc (binop_expected_type op, t_l.typ) in
     let t_r = type_expr env r in
     let _ = verify_operand_type op r.loc r.desc (Some exp_type, t_r.typ) in
     TEbinop (op, t_l, t_r), type_of_binop op, false

and check_expr_list env = function
  | [] ->
     []
  | [p] ->
     [ type_expr env p ]
  | _ as pl ->
     List.map
       (fun p -> let t_p = type_expr env p in
                 match t_p.typ with
                 | TTunit ->
                    type_error p.loc
                      (Format.asprintf "%a used as value" Utils.string_of_expr p.desc)
                 | TTtuple tl ->
                    type_error p.loc
                      (Format.asprintf "multiple-value %a in single-value context"
                         Utils.string_of_expr p.desc)
                 | _ ->
                    t_p
       ) pl
    
(* Checks if the types of the actual params match the expected ones (formal params) *)
and check_fun_params env f loc formals actuals =
  let ty_formals = List.map snd formals in
  match actuals with
  | [act] ->
     let t_act = type_expr env act in
     let ty_actuals =
       match t_act.typ with
       | TTtuple l ->
          l
       | TTunit ->
          type_error act.loc (Format.asprintf "%a used as value" Utils.string_of_expr act.desc)
       | _ as t ->
          [t]
     in
     let cmp_act_form = compare (List.length ty_actuals) (List.length ty_formals) in
     if cmp_act_form = 0 then begin
         List.iter2
           (fun ff aa ->
             match ff, aa with
             | t_f, t_a when t_f = t_a ->
                ()
             | TTpointer _, TTnil ->
                ()
             | TTuntyped, (TTint | TTstring | TTbool | TTstruct _ | TTpointer _) ->
                ()
             | TTuntyped, _ ->
                assert false
             | t_f, t_a ->
                type_error act.loc
                  (Format.asprintf "cannot use %a value as type %a in argument to %s"
                     Utils.string_of_type t_a Utils.string_of_type t_f f)
           ) ty_formals ty_actuals;  
         [t_act]
       end else
       let msg = if cmp_act_form > 0 then "too many" else "not enough" in
       type_error loc
         (Format.asprintf "%s arguments in call to %s\n\t\t have %a\n\t\t want %a"
            msg f Utils.string_of_type_list ty_actuals Utils.string_of_type_list ty_formals)
  | _  ->
     let cmp_act_form = compare (List.length actuals) (List.length ty_formals) in
     if cmp_act_form = 0 then begin
         List.map2
           (fun ty_form act -> 
             let t_act = type_expr env act in
             match ty_form, t_act.typ with
             | t_f, t_a when t_f = t_a ->
                t_act
             | TTpointer _, TTnil ->
                (* TTnil's "unification" *)
                { t_act with typ = ty_form }
             | _, TTunit ->
                type_error act.loc
                  (Format.asprintf "%a used as value" Utils.string_of_expr act.desc)
             | _, TTtuple _ ->
                type_error act.loc
                  (Format.asprintf "multiple-value %a in single-value context"
                     Utils.string_of_expr act.desc)
             | t_f, t_a ->
                type_error act.loc
                  (Format.asprintf "cannot use %a (type %a) as type %a in argument to %s"
                     Utils.string_of_expr act.desc Utils.string_of_type t_a Utils.string_of_type t_f f)
           ) ty_formals actuals 
       end else
       let msg = if cmp_act_form > 0 then "too many" else "not enough" in
       type_error loc
         (Format.asprintf "%s arguments in call to %s\n\t\t have %a\n\t\t want %a"
            msg f Utils.string_of_type_list (List.map (fun act -> (type_expr env act).typ) actuals)
            Utils.string_of_type_list ty_formals)

let check_assignment env loc to_be_assigned values =
  let l_assigned = List.length to_be_assigned in
  match values with
  | [value] ->
     let te_value = type_expr env value in
     let t_values =
       match te_value.typ with
       | TTtuple vs ->
          vs
       | TTunit ->
          type_error value.loc (Format.asprintf "%a used as value" Utils.string_of_expr value.desc)
       | _ as vs ->
          [vs]
     in
     let l_values = List.length t_values in
     if l_assigned = l_values then begin
         List.iter2
           (fun t_ass vv ->
             match t_ass.typ, vv with
             | t_a, t_v when t_a = t_v ->
                ()
             | TTpointer _, TTnil ->
                ()
             | TTuntyped, (TTint | TTstring | TTbool | TTstruct _ | TTpointer _) ->
                ()
             | _, (TTunit | TTuntyped | TTtuple _) ->
                assert false
             | TTuntyped, TTnil ->
                type_error value.loc (Format.asprintf "use of untyped nil")
             | t_a, t_v ->
                type_error value.loc
                  (Format.asprintf "cannot assign %a to %a (type %a) in multiple assignment"
                     Utils.string_of_type t_v Utils.string_of_texpr t_ass.tdesc
                     Utils.string_of_type t_a)
           ) to_be_assigned t_values;
         match t_values with
         | [t_val] when t_val = TTnil ->
            (* TTnil's "unification" *)
            [{ te_value with typ = (List.hd to_be_assigned).typ }]
         | _ ->
            [te_value]
       end else
       type_error loc
         (Format.asprintf "assignment mismatch: %d variable(s) but %a returns %d value(s)"
            l_assigned Utils.string_of_expr value.desc l_values)
  | _  ->
     let l_values = List.length values in
     if l_assigned = l_values then begin
         List.map2
           (fun t_ass val_ ->
             let t_val = type_expr env val_ in
             match t_ass.typ, t_val.typ with
             | t_a, t_v when t_a = t_v ->
                t_val
             | TTpointer _, TTnil ->
                (* TTnil's "unification" *)
                { t_val with typ = t_ass.typ }
             | TTuntyped, (TTint | TTstring | TTbool | TTstruct _ | TTpointer _) ->
                t_val
             | _, TTunit ->
                type_error val_.loc
                  (Format.asprintf "%a used as value" Utils.string_of_expr val_.desc)
             | _, TTtuple _ ->
                type_error val_.loc
                  (Format.asprintf "multiple-value %a in single-value context"
                     Utils.string_of_expr val_.desc)
             | TTuntyped, TTnil ->
                type_error val_.loc (Format.asprintf "use of untyped nil")
             | t_a, t_v ->
                type_error val_.loc
                  (Format.asprintf "cannot use %a (type %a) as type %a in assignment"
                     Utils.string_of_expr val_.desc Utils.string_of_type t_v Utils.string_of_type t_a)
           ) to_be_assigned values 
       end else
       type_error loc
         (Format.asprintf "assignment mismatch: %d variable(s) but %d value(s)" l_assigned l_values)
     
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
  { tdesc = TEnil; typ; is_assignable = false; loc = dummy_loc }
     
let rec type_shstmt env b_vars level = function
  | Ast.Ieval e ->
     begin
       match e.desc with
       | Ecst _ | Eident _ | Eselect _ | Eunop _ | Ebinop _ ->
          type_error e.loc (Format.asprintf "%a evaluated but not used" Utils.string_of_expr e.desc)
       | Ecall (f, actuals) ->
          let t_call = type_expr env e in
          let actuals = match t_call.tdesc with | TEcall (_, act) -> act | _ -> assert false in
          b_vars, TScall (f, actuals)
       | Eprint el ->
          let t_print = type_expr env e in
          let exprs = match t_print.tdesc with | TEprint exprs -> exprs | _ -> assert false in
          b_vars, TSprint exprs
     end
  | Ast.Iincr e | Idecr e as i_d->
     let string_of_op = match i_d with | Iincr _ -> "++" | Idecr _ -> "--" | _ -> assert false in
     let t_e = type_expr env e in
     if t_e.is_assignable then
       if t_e.typ = TTint
       then b_vars, match i_d with | Iincr _ -> TSincr t_e | Idecr _ -> TSdecr t_e | _ -> assert false
       else type_error e.loc
              (Format.asprintf "invalid operation %a%s (non-numeric type %a)"
                 Utils.string_of_expr e.desc string_of_op Utils.string_of_type t_e.typ)
     else type_error e.loc (Format.asprintf "cannot assign to %a" Utils.string_of_expr e.desc)
     
  | Ast.Iassign (assigned_s, values) ->
     let t_assigned_s = List.map
                          (fun ass -> if ass.Ast.desc = Ast.Eident "_" then under_texpr
                                      else
                                        let t_ass = type_expr env ass in
                                        if not t_ass.is_assignable then
                                          type_error ass.loc
                                            (Format.asprintf "cannot assign to %a"
                                               Utils.string_of_expr ass.desc);
                                        t_ass
                          ) assigned_s in
     let loc = try (List.hd values).loc with Failure _ -> assert false in
     let t_values = check_assignment env loc t_assigned_s values in
     b_vars, TSassign (t_assigned_s, t_values)
  | Ast.Ideclare (vars, values) ->
     (* Behaviour in Go: this can also assign values provided at least one left var is new *)
     (* the 'local' environment is updated by the declaration statement *)
     let untyped_vars = List.map expr_placeholder vars in
     let loc = try (List.hd values).loc with Failure _ -> assert false in
     let t_values = check_assignment env loc untyped_vars values in
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
     let rec update_var_map (var_map:Ty_ast.tvar Ty_ast.Smap.t) t_vars = function
       | [], [] ->
          var_map, List.rev t_vars
       | [], _ | _, [] ->
          assert false
       | (id, loc) :: var_names, typ :: types when id = "_" ->
          (* variable "_" is not added to the local environment *)
          update_var_map var_map (underscore :: t_vars) (var_names, types)
       | (id, loc) :: var_names, typ :: types ->
          (* check name unicity *)
          try
            let previous_decl = Smap.find id var_map in
            let line, fst_char, last_char = Utils.position_of_loc previous_decl.loc in
            type_error loc
              (Format.sprintf
                 "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
                 id fst_char last_char line)
          with Not_found ->
            let n_var = new_var id level ~loc typ in
            update_var_map (Smap.add id n_var var_map) (n_var :: t_vars) (var_names, types)
     in
     let b_vars, t_assigned_s = update_var_map b_vars [] (vars, types) in
     b_vars, TSdeclare (t_assigned_s, t_values)

let rec type_stmt env b_vars level = function
  | Ast.Snop ->
     b_vars, TSnop
  | Ast.Sexec shstmt ->
     type_shstmt env b_vars level shstmt
  | Ast.Sblock b ->
     let vars, stmts = type_nested_stmt env b (level + 1) in 
     b_vars, TSblock { vars; stmts; level }
  | Ast.Sif (cond, bif, belse) ->
     (* condition *)
     let t_cond = type_expr env cond in
     if t_cond.typ <> TTbool then
       type_error cond.loc
         (Format.asprintf "non-bool %a (type %a) used as if condition"
            Utils.string_of_expr cond.desc Utils.string_of_type t_cond.typ);
     (* branch then *)
     let if_vars, if_stmts = type_nested_stmt env bif (level + 1) in
     (* branch else *)
     let else_vars, else_stmts = type_nested_stmt env belse (level + 1) in
     b_vars, TSif (t_cond, { vars = if_vars; stmts = if_stmts; level },
                   { vars = else_vars; stmts = else_stmts; level })
  | Ast.Sinit (vars, ty, values) ->
     begin
       match values with
       | [] -> (* declaration *)
          (* syntax error if not given *)
          let typ = match ty with | Some t -> type_of_ast_typ t | None -> assert false in
          let rec update_var_map (var_map:Ty_ast.tvar Ty_ast.Smap.t) t_vars = function
            | [] ->
               var_map, List.rev t_vars
            | (id, loc) :: var_names when id = "_" ->
               (* variable "_" is not added to the local environment *)
               update_var_map var_map (underscore :: t_vars) var_names
            | (id, loc) :: var_names ->
               (* check name unicity *)
               try
                 let previous_decl = Smap.find id var_map in
                 let line, fst_char, last_char = Utils.position_of_loc previous_decl.loc in
                 type_error loc
                   (Format.sprintf
                      "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
                      id fst_char last_char line)
               with Not_found ->
                 let n_var = new_var id level ~loc typ in
                 update_var_map (Smap.add id n_var var_map) (n_var :: t_vars) var_names
                 
          in
          let b_vars, t_assigned_s = update_var_map b_vars [] vars in
          b_vars, TSdeclare (t_assigned_s, [])
       | _ -> (* declare-and-assign *)
          let typ = match ty with | Some t -> Some (type_of_ast_typ t) | None -> None in
          let vars_ph =
            match typ with
            | Some typ ->
               List.map (expr_placeholder ~typ) vars
            | None ->
               List.map expr_placeholder vars
          in
          let loc = (List.hd values).loc in
          let t_values = check_assignment env loc vars_ph values in
          let types =
            match t_values with
            | [{ tdesc; typ = TTtuple types; is_assignable; loc }] ->
               types
            | _ as types ->
               List.map (fun t_e -> t_e.typ) types
          in
          let rec update_var_map (var_map:Ty_ast.tvar Ty_ast.Smap.t) t_vars = function
            | [], [] ->
               var_map, List.rev t_vars
            | [], _ | _, [] ->
               assert false
            | (id, loc) :: var_names, typ :: types when id = "_" ->
               (* variable "_" is not added to the local environment *)
               update_var_map var_map (underscore :: t_vars) (var_names, types)
            | (id, loc) :: var_names, typ :: types ->
               (* check name unicity *)
               try
                 let previous_decl = Smap.find id var_map in
                 let line, fst_char, last_char = Utils.position_of_loc previous_decl.loc in
                 type_error loc
                   (Format.sprintf
                      "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
                      id fst_char last_char line)
               with Not_found ->
                 let n_var = new_var id level ~loc typ in
                 update_var_map (Smap.add id n_var var_map) (n_var :: t_vars) (var_names, types)
          in
          let b_vars, t_assigned_s = update_var_map b_vars [] (vars, types) in
          b_vars, TSdeclare (t_assigned_s, t_values)  
     end
  | Ast.Sreturn (exps) ->
     b_vars, TSreturn (check_expr_list env exps)
  | Ast.Sfor (init, cond, post, body) ->
     begin
       (* init statement*)
       let outer_vars, t_init, env =
         match init with
         | None ->
            Smap.empty, TSnop, env 
         | Some st ->
            let out_vars, t_init = type_shstmt env Smap.empty (level + 1) st in
            out_vars, t_init, update_env env out_vars t_init
       in
       (* condition *)
       let t_cond = type_expr env cond in
       if t_cond.typ <> TTbool then
         type_error cond.loc
           (Format.asprintf "non-bool %a (type %a) used as for condition"
              Utils.string_of_expr cond.desc Utils.string_of_type t_cond.typ);
       (* post statement & body *)
       let for_level = if t_init = TSnop then level else level + 1 in
       let for_vars, for_stmts =
         match post with
         | None ->
            type_nested_stmt env body (for_level + 1)
         | Some st ->
            type_nested_stmt env body ~ending_stmt:(Ast.Sexec st) (for_level + 1)
       in
       let for_stmt = TSfor (t_cond, { vars = for_vars; stmts = for_stmts; level = for_level }) in
       b_vars,
       if t_init = TSnop then for_stmt
       else TSblock { vars = outer_vars; stmts = t_init :: for_stmts; level }
     end
    
and type_nested_stmt env stmts ?ending_stmt level =
  let rec loop env var_map stmts ending_stmt t_stmts level =
    match stmts, ending_stmt with
    | [], None ->
       var_map, List.rev t_stmts
    | [], Some st ->
       loop env var_map [st] None t_stmts level
    | st :: stmts, _ ->
       let var_map, t_st = type_stmt env var_map level st in
       (* the 'global' environment is updated by the block *)
       let env = update_env env var_map t_st in
       loop env var_map stmts ending_stmt (t_st :: t_stmts) level
  in loop env Smap.empty stmts ending_stmt [] level

let rec type_vars_of_decl (vars, typ) typ_list var_map msg =
  match vars with
  | [] ->
     typ_list, var_map
  | (id, loc) :: vars ->
     try
       let _ = Smap.find id var_map in
       type_error loc (Format.asprintf "duplicate %s %s" msg id)
     with Not_found ->
       type_vars_of_decl (vars, typ) ((id, typ) :: typ_list) (Smap.add id typ var_map) msg

let type_vars_list msg =
  List.fold_left
    (fun (typ_list, var_map) (vars, typ) ->
      let typ = type_of_ast_typ typ in
      type_vars_of_decl (vars, typ) typ_list var_map msg
    ) ([], Smap.empty)
  
let rec identify_declarations structs functions = function
  | [] ->
     structs, functions
  | Ast.Dstruct ((name, loc), fields) :: decls ->
     begin
       try let _, previous_loc = Smap.find name !struct_env in
           let line, fst_char, last_char = Utils.position_of_loc previous_loc in
           type_error loc
             (Format.asprintf
                "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
                name fst_char last_char line)
       with Not_found ->
         struct_env := Smap.add name (Smap.empty, loc) !struct_env
     end;
     identify_declarations ((name, loc, fields) :: structs) functions decls
    | Ast.Dfunc ((name, loc), args, rtype, body)  :: decls ->
       identify_declarations structs ((name, loc, args, rtype, body) :: functions) decls

let check_fun_sign (name, loc, args, rtype, body) =
  try
    let _, _, _, previous_loc = Smap.find name !func_env in
    let line, fst_char, last_char = Utils.position_of_loc previous_loc in
    type_error loc
      (Format.asprintf
         "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
         name fst_char last_char line)
  with Not_found ->
    (* check arguments *)
    let args_list, var_map = type_vars_list "argument" args in
    (* check return type *)
    let t_rtype =
      match List.map type_of_ast_typ rtype with
      | [] ->
         TTunit
      | [t] ->
         t
      | _ as ts ->
         TTtuple ts
    in
    let t_args = List.rev args_list in
    func_env := Smap.add name (t_args, t_rtype, Untyped body, loc) !func_env

let type_struct_fields (name, loc, fields) =
  (* type fields *)
  let _, field_map = type_vars_list "field" fields in
  struct_env := Smap.add name (field_map, loc) !struct_env

let type_fun_body (name, _, _, _, _) =
  let args, rtype, ubody, loc = Smap.find name !func_env in
  let env = List.fold_left
              (fun env (id, t) -> Smap.add id (new_var id 0 t) env) Smap.empty args in
  let body = match ubody with Untyped b -> b | Typed _ -> assert false in
  let vars, stmts = type_nested_stmt env body 1 in
  (* check return *)
  let tbody = Typed { vars; stmts; level = 0 } in
  func_env := Smap.add name (args, rtype, tbody, loc) !func_env

let check_recursive_struct root =
  let rec dfs_scan (fields, loc) =
    Smap.iter
      (fun _ -> function
        | TTstruct curr when curr = root -> 
           type_error loc  (Format.asprintf "invalid recursive type %s" root)
        | TTstruct curr ->
           dfs_scan (Smap.find curr !struct_env)
        | _ ->
           ()
      ) fields
  in dfs_scan

exception Found_main
   
let check_func_main functions =
  try
    List.iter (fun (name, loc, args, rtype, _) ->
        if name = "main" then
          if args = [] && rtype = [] then raise Found_main
          else type_error loc "func main must have no arguments and no return values"
      ) functions;
    type_error dummy_loc "function main is undeclared in the main package"
  with Found_main -> ()

let rec scan_texpr env use_queue expr =
  match expr.tdesc with
  | TEint _ | TEstring _ | TEbool _ | TEnil | TEnew _ | TEident "_" ->
     env, use_queue
  | TEident id ->
     begin
       try
         let occ, loc = Smap.find id env in
         Smap.add id (occ + 1, loc) env, use_queue
       with Not_found ->
         env, id :: use_queue
     end
  | TEselect (str, _) ->
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
  | TSprint texps ->
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
     let env, use_queue = scan_texpr_list env use_queue to_be_assigned in
     let env, use_queue = scan_texpr_list env use_queue values in
     env, use_queue, false
  | TSdeclare (vars, values) ->
     let env, use_queue = scan_texpr_list env use_queue values in
     List.fold_left
       (fun env v ->
         if v.id = "_" then env else Smap.add v.id (0, v.loc) env
       ) env vars, use_queue, false
  | TSreturn tactuals ->
     let env, use_queue = scan_texpr_list env use_queue tactuals in
     begin
       match tactuals with
       | [tact] ->
          let act_rtype =
            match tact.typ with
            | TTtuple tl ->
               tl
            | _ as t ->
               [t]
          in
          let cmp_exp_act = compare (List.length exp_rtype) (List.length act_rtype) in
          if cmp_exp_act = 0 then begin
              List.iter2
                (fun exp act ->
                  (* TODO use match + TTnil's unification *)
                  (* unify these matches into a single fct returning the act_val??? *)
                  if act <> exp then
                    type_error tact.loc
                      (Format.asprintf "cannot use %a value as type %a in return argument"
                         Utils.string_of_type act Utils.string_of_type exp)
                ) exp_rtype act_rtype;
              let env, use_queue = scan_texpr env use_queue tact in
              env, use_queue, true
            end else
            let msg = if cmp_exp_act > 0 then "not enough" else "too many" in
            type_error loc
              (Format.asprintf "%s arguments to return\n\t have %a\n\t want %a"
                 msg Utils.string_of_type_list act_rtype Utils.string_of_type_list exp_rtype)
       | _ ->
          let cmp_exp_act = compare (List.length exp_rtype) (List.length tactuals) in
          if cmp_exp_act = 0 then begin
              let env, use_queue =
                List.fold_left2
                  (fun (env, queue) exp act ->
                    if act.typ <> exp then
                      type_error act.loc
                        (Format.asprintf "cannot use %a (type %a) as type %a in return argument"
                           Utils.string_of_texpr act.tdesc Utils.string_of_type act.typ
                           Utils.string_of_type exp);
                    scan_texpr env queue act
                  ) (env, use_queue) exp_rtype tactuals in
              env, use_queue, true
            end else
            let msg = if cmp_exp_act > 0 then "not enough" else "too many" in
            type_error loc
              (Format.asprintf "%s arguments to return\n\t have %a\n\t want %a"
                 msg Utils.string_of_type_list (List.map (fun te -> te.typ) tactuals)
                 Utils.string_of_type_list exp_rtype)
     end
  | TSfor (cond, body) ->
(* TODO *)
     env, use_queue, true

and scan_block loc env use_queue rtype block =
  let b_env, b_queue, final =
    List.fold_left (fun (env, use_queue, _) st -> scan_tstmt loc env use_queue rtype st)
      (Smap.empty, [], false) block.stmts in
  Smap.iter
    (fun id (occ, loc) -> if occ = 0 then
                            type_error loc (Format.sprintf "%s declared and not used" id)) b_env;
  let env, use_queue = reduce_queue env use_queue b_queue in
  env, use_queue, final
    
let check_fun_return name (args, rtype, body, loc) =
  let env = List.fold_left (fun env (id, _) -> Smap.add id (0, loc) env) Smap.empty args in
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
  let env, use_queue, final = scan_block loc env [] exp_rtype block in
  if not final && exp_rtype <> [] then type_error loc "missing return at end of function"
                   
let type_file file =
  import_fmt := fst file.Ast.imp;
  let import_loc = snd file.Ast.imp in
  let structs, functions = identify_declarations [] [] file.Ast.decls in
  List.iter check_fun_sign functions;
  List.iter type_struct_fields structs;
  List.iter type_fun_body functions;
  Smap.iter check_fun_return !func_env;
  Smap.iter check_recursive_struct !struct_env;
  Smap.iter check_fun_return !func_env;
  check_func_main functions;
  if !import_fmt <> !used_fmt then
    type_error import_loc "imported and not used: \"fmt\"";
  { structs = !struct_env; functions = !func_env }            

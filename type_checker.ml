
open Ty_ast

exception Typing_error of Ast.loc * string

(* module Smap = Map.Make(String) *)

let struct_env : Ty_ast.struct_ Smap.t ref = ref Smap.empty
let func_env : Ty_ast.func Smap.t ref = ref Smap.empty

let length_of_type = function
  | Tint | Tstring | Tbool | Tnil | Tstruct _ | Tpointer _ ->
     1
  | Tunit ->
     0
  | Ttuple tl -> List.length tl

let rec is_tau_kind = function
  | Tint | Tstring | Tbool ->
     true
  | Tnil | Tunit | Ttuple _ ->
     false
  | Tstruct s ->
     Smap.mem s !struct_env
  | Tpointer t ->
     is_tau_kind t
                                      
let rec string_of_type_list fmt tl =
  Format.fprintf fmt "(";
  List.iteri
    (fun i t -> Format.fprintf fmt "%s%a" (if i > 0 then "," else "") string_of_type t) tl;
  Format.fprintf fmt ")"
  
and string_of_type fmt = function
  | Tint ->
     Format.fprintf fmt "int"
  | Tbool ->
     Format.fprintf fmt "bool"
  | Tstring ->
     Format.fprintf fmt "str"
  | Tnil ->
     Format.fprintf fmt "nil"
  | Tunit ->
     Format.fprintf fmt "unit"
  | Tstruct s ->
     Format.fprintf fmt "%s" s
  | Ttuple tl ->
     string_of_type_list fmt tl
  | Tpointer t ->
     Format.fprintf fmt "*%a" string_of_type t

let type_of_unop exp op t_exp =
  match op, t_exp.typ with
  | Ast.Unot, Tbool ->
     Tbool, false
  | Ast.Uneg, Tint ->
     Tint, false
  | Ast.Udref, Tpointer t ->
     t, exp.Ast.desc <> Ast.Ecst Ast.Cnil
  | Ast.Uaddr, t ->
     if t_exp.is_assignable then Tpointer t, false
     else raise (Typing_error (exp.loc, Format.asprintf "cannot take the address of %a"
                                          Utils.string_of_expr exp.desc))
  | Ast.Udref, Tnil ->
     raise (Typing_error (exp.loc, Format.asprintf "invalid indirect of nil"))
  | (Ast.Unot, (Tint | Tstring | Tnil | Tunit | Tstruct _ | Ttuple _ | Tpointer _))
  | (Ast.Uneg, (Tstring | Tbool | Tnil | Tunit | Tstruct _ |  Ttuple _ | Tpointer _))
  | (Ast.Udref, (Tint | Tstring | Tbool | Tunit | Tstruct _ |Ttuple _)) ->
     raise (Typing_error
              (exp.loc, Format.asprintf "invalid operation %a %a"
                      Utils.string_of_unop op string_of_type t_exp.typ))

let expected_type = function
  | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv | Ast.Bmod | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge ->
     Some Tint
  | Ast.Beq | Ast.Bneq ->
     None
  | Ast.Band | Ast.Bor ->
     Some Tbool
  
let verify_operand_type op loc operand = function
  | None, t | Some Tnil, (Tpointer _ as t)  | Some (Tpointer _ as t), Tnil ->
     t
  | Some Tnil, Tnil ->
     raise (Typing_error
              (loc, Format.asprintf "invalid operation nil %a nil (operator %a not defined on nil)"
                     Utils.string_of_binop op Utils.string_of_binop op))
  | Some exp_typ, typ when typ = exp_typ ->
     typ
  | Some exp_typ, typ ->
     raise (Typing_error
              (loc, Format.asprintf "cannot convert %a (type %a) to type %a"
                      Utils.string_of_expr operand string_of_type typ string_of_type exp_typ))
    
let new_var id typ =
  { id; level = 0; offset = 0; typ }
  
let rec type_expr env (e:Ast.expr) =
  let tdesc, typ, is_assignable = compute_type env e in
  { tdesc; typ; is_assignable; loc = e.loc }
  
and compute_type (env:Ty_ast.var Smap.t) e =
  match e.desc with
  | Ast.Ecst (Cint n) ->
     TEint n, Tint, false
  | Ast.Ecst (Cstring s) ->
     TEstring s, Tstring, false
  | Ast.Ecst (Cbool b) ->
     TEbool b, Tbool, false
  | Ast.Ecst Cnil ->
     TEnil, Tnil, false (* TODO *)
  | Ast.Eident v ->
     begin
       try
         TEident v, (let feats = Smap.find v env in feats.typ), true
       with Not_found -> raise (Typing_error (e.loc, Format.sprintf "undefined %s" v))
     end
  | Ast.Eselect (str, fd, fd_loc) ->
     begin
       let t_str = type_expr env str in
       match t_str.typ with
       | Tstruct s | Tpointer (Tstruct s) as t ->
          begin
            try
              let fields = Smap.find s !struct_env in
              TEselect (s, fd), Smap.find fd fields, t_str.is_assignable
            with Not_found ->
              raise (Typing_error
                       (fd_loc, Format.asprintf "%a.%s undefined (type %a has no field %s)"
                                   Utils.string_of_expr str.desc fd string_of_type t fd))   
          end
       | Tint | Tstring | Tbool | Tunit | Ttuple _ | Tpointer _ as t ->
          raise (Typing_error
                   (str.loc, Format.asprintf "%a.%s undefined (type %a has no field %s)"
                               Utils.string_of_expr str.desc fd string_of_type t fd))   
       | Tnil ->
          raise (Typing_error (str.loc, Format.sprintf "nil pointer dereference"))   
     end
  | Ast.Ecall (f, actuals) ->
     begin
       try
         let formals, rtype, _ = Smap.find f !func_env in
         let actuals : Ty_ast.texpr list = check_fun_params env f e.loc formals actuals in
         TEcall (f, actuals), rtype, false
       with Not_found -> raise (Typing_error (e.loc, Format.sprintf "undefined %s" f))
     end
  | Ast.Eprint el ->
     TEprint (check_expr_list env el), Tunit, false
  | Ast.Eunop (op, e) ->
     let t_e = type_expr env e in
     let typ, is_assignable = type_of_unop e op t_e in
     TEunop (op, t_e), typ, is_assignable
  | Ast.Ebinop (op, l, r) ->
     let t_l = type_expr env l in
     let exp_type = verify_operand_type op l.loc l.desc (expected_type op, t_l.typ) in
     let t_r = type_expr env r in
     let exp_type = verify_operand_type op r.loc r.desc (Some exp_type, t_r.typ) in
     TEbinop (op, t_l, t_r), exp_type, false

and check_expr_list env = function
  | [] ->
     []
  | [p] ->
     [ type_expr env p ]
  | _ as pl ->
     List.map
       (fun p -> let t_p = type_expr env p in
                 match t_p.typ with
                 | Tunit ->
                    raise (Typing_error
                             (p.loc, Format.asprintf "%a used as value" Utils.string_of_expr p.desc))
                 | Ttuple tl ->
                    raise (Typing_error
                             (p.loc,
                              Format.asprintf "multiple-value %a in single-value context"
                                Utils.string_of_expr p.desc))
                 | _ ->
                    t_p
       ) pl
     
and check_fun_params env f loc formals actuals =
  let t_formals = match formals with | Ttuple l -> l | _ as t -> [t] in
  match actuals with
  | [act] ->
     let t_act = type_expr env act in
     let t_actuals =
       match t_act.typ with
       | Ttuple l ->
          l
       | Tunit ->
          raise (Typing_error
                   (act.loc, Format.asprintf "%a used as value" Utils.string_of_expr act.desc))
       | _ as t ->
          [t]
     in
     let cmp_act_form = compare (List.length t_actuals) (List.length t_formals) in
     if cmp_act_form = 0 then begin
         List.iter2
           (fun ff aa -> match ff, aa with
                         | t_f, t_a when t_f = t_a ->
                            ()
                         | Tpointer _, Tnil ->
                            ()
                         | t_f, t_a ->
                            raise (Typing_error 
                                   (act.loc, Format.asprintf
                                               "cannot use %a value as type %a in argument to %s"
                                               string_of_type t_a string_of_type t_f f)))
           t_formals t_actuals;  
         [t_act]
       end else
       let msg = if cmp_act_form > 0 then "too many" else "not enough" in
       raise (Typing_error 
                (loc, Format.asprintf
                        "%s arguments in call to %s\n\t\t have %a\n\t\t want %a"
                        msg f string_of_type_list t_actuals string_of_type_list t_formals))
  | _  ->
     let cmp_act_form = compare (List.length actuals) (List.length t_formals) in
     if cmp_act_form = 0 then begin
         List.map2
           (fun form act -> let t_act = type_expr env act in
                            match form, t_act.typ with
                            | t_f, t_a when t_f = t_a ->
                               t_act
                            | Tpointer _, Tnil ->
                               (* Tnil's "unification" *)
                               { t_act with typ = form }
                            | _, Tunit ->
                               raise (Typing_error
                                        (act.loc, Format.asprintf "%a used as value"
                                                    Utils.string_of_expr act.desc))
                            | t_f, t_a ->
                               raise (Typing_error 
                                        (act.loc, Format.asprintf
                                                "cannot use %a (type %a) as type %a in argument to %s"
                                                Utils.string_of_expr act.desc string_of_type t_a
                                                string_of_type t_f f))
           ) t_formals actuals 
       end else
       let msg = if cmp_act_form > 0 then "too many" else "not enough" in
       raise (Typing_error 
                (loc, Format.asprintf
                        "%s arguments in call to %s\n\t\t have %a\n\t\t want %a"
                        msg f string_of_type_list
                        (List.map (fun act -> (type_expr env act).typ) actuals)
                        string_of_type_list t_formals))

let check_assignment env loc to_be_assigned values =
  let l_assigned = List.length to_be_assigned in
  match values with
  | [value] ->
     let t_value = type_expr env value in
     let t_values =
       match t_value.typ with
       | Ttuple vs ->
          vs
       | Tunit ->
          raise (Typing_error
                   (value.loc, Format.asprintf "%a used as value" Utils.string_of_expr value.desc))
       | _ as vs ->
          [vs]
     in
     let l_values = List.length t_values in
     if l_assigned = l_values then begin
         List.iter2
           (fun t_ass vv -> match t_ass.typ, vv with
                               | t_a, t_v when t_a = t_v ->
                                  ()
                               | Tpointer _, Tnil ->
                                  ()
                               | t_a, t_v ->
                                  raise (Typing_error 
                                           (value.loc, Format.asprintf
                                                         "cannot assign %a to %a (type %a) in multiple assignment"
                                                         string_of_type t_v Utils.string_of_texpr t_ass.tdesc string_of_type t_a)))
           to_be_assigned t_values;  
         [t_value]
       end else
       raise (Typing_error 
                (loc, Format.asprintf
                        "assignment mismatch: %d variable(s) but %d value(s)"
                        l_assigned l_values))
  | _  ->
     let l_values = List.length values in
     if l_assigned = l_values then begin
         List.map2
           (fun t_ass val_ -> let t_val = type_expr env val_ in
                              match t_ass.typ, t_val.typ with
                              | t_a, t_v when t_a = t_v ->
                                 t_val
                              | Tpointer _, Tnil ->
                                 (* Tnil's "unification" *)
                                 { t_val with typ = t_ass.typ }
                              | t_a, t_v ->
                                 raise (Typing_error 
                                          (val_.loc, Format.asprintf
                                                       "cannot use %a (type %a) as type %a in assignment"
                                                       Utils.string_of_expr val_.desc
                                                       string_of_type t_v
                                                       string_of_type t_a))
           ) to_be_assigned values 
       end else
       raise (Typing_error 
                (loc, Format.asprintf
                        "assignment mismatch: %d variable(s) but %d value(s)" l_assigned l_values))
                
       
let rec type_stmt (env:Ty_ast.var Smap.t) b_vars level = function
  | Ast.Snop ->
     b_vars, TSnop
  | Ast.Sexec (Ieval e) ->
     begin
       match e.desc with
       | Ecst _ | Eident _ | Eselect _ | Eunop _ | Ebinop _ ->
          raise (Typing_error
                   (e.loc, Format.asprintf "%a evaluated but not used" Utils.string_of_expr e.desc))
       | Ecall (f, actuals) ->
          let t_call = type_expr env e in
          let actuals = match t_call.tdesc with | TEcall (_, act) -> act | _ -> assert false in
          b_vars, TScall (f, actuals)
       | Eprint el ->
          let t_print = type_expr env e in
          let exprs = match t_print.tdesc with | TEprint exprs -> exprs | _ -> assert false in
          b_vars, TSprint exprs
     end
  | Ast.Sexec (Iincr e|Idecr e as i_d) ->
     let string_of_op = match i_d with | Iincr _ -> "++" | Idecr _ -> "--" | _ -> assert false in
     let t_e = type_expr env e in
     if t_e.is_assignable then
       if t_e.typ = Tint
       then b_vars, match i_d with | Iincr _ -> TSincr t_e | Idecr _ -> TSdecr t_e | _ -> assert false
       else raise (Typing_error
                     (e.loc,
                      Format.asprintf "invalid operation %a%s (non-numeric type %a)"
              Utils.string_of_expr e.desc string_of_op string_of_type t_e.typ))
     else raise (Typing_error
                   (e.loc, Format.asprintf "cannot assign to %a" Utils.string_of_expr e.desc))
  | Ast.Sexec (Iassign (assigned_s, values)) ->
     let t_assigned_s = List.map
                          (fun ass -> let t_ass = type_expr env ass in
                                      if not t_ass.is_assignable then
                                        raise (Typing_error
                                                 (ass.loc, Format.asprintf "cannot assign to %a"
                                                             Utils.string_of_expr ass.desc));
                                      t_ass
                          ) assigned_s in
     let loc = try (List.hd values).loc with Failure _ -> assert false in
     let t_values = check_assignment env loc t_assigned_s values in
     b_vars, TSassign (t_assigned_s, t_values)
  | Ast.Sexec (Ideclare (vars, values)) ->
     (* the 'local' environment is updated by the declaration statement *)
     assert false (* TODO *)
  | Ast.Sblock b ->
     (* Check that each local variable is indeed used at least once *)
     let rec type_nested_stmt env var_map stmts t_stmts =
       match stmts with
       | [] ->
          var_map, List.rev t_stmts
       | st :: stmts ->
          let var_map, t_st = type_stmt env var_map (level + 1) st in
          (* the 'global' environment is updated by the block *)
          let env = 
            match t_st with
            | TSdeclare (var_list, values) ->
               List.fold_left
                 (fun env v -> match v.tdesc with
                               | TEident var ->
                                  begin
                                    try Smap.add var (Smap.find var var_map) env
                                    with Not_found -> assert false
                                  end
                               | _ ->
                                  assert false
                 ) env var_list
            | _ ->
               env
          in
          type_nested_stmt env var_map stmts (t_st :: t_stmts)
     in
     let vars, stmts = type_nested_stmt env Smap.empty b [] in 
     b_vars, TSblock { vars; stmts; level }
  | Ast.Sif (cond, bif, belse) ->
     assert false (* TODO *)
  | Ast.Sinit (vars, ty, values) ->
     assert false (* TODO *)
  | Ast.Sreturn (exps) ->
     b_vars, TSreturn (check_expr_list env exps)
  | Ast.Sfor (init, cond, post, block) ->
     (* begin
      *   match init with
      *   | None ->
      *      ...
      *   | Some ->
      *      ...
      *   b_vars, TSfor (cond, tblock)
      * 
      * end *)
     assert false (* TODO *)


open Ast
open Asg

exception Type_error of Utils.loc * string
exception Found_main
        
(* Function return conventions:
 *** 0 -> unit
 *** 1 -> tau
 *** 2+ -> list of tau 
 *)

let struct_env = ref Utils.Smap.empty
let func_env = ref Utils.Smap.empty
let import_fmt = ref false
let used_fmt = ref false
(* used for block enumeration *)
let block_number = ref (-1)

let type_error loc msg =
  raise (Type_error (loc, msg))

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
     try
       let _ = Utils.Smap.find str !struct_env in
       TTstruct str
     with Not_found ->
       type_error loc (Format.asprintf "undefined: %s" str)

let rec string_of_type_list fmt tl =
  Format.fprintf fmt "(";
  List.iteri (fun i t ->
      Format.fprintf fmt "%s%a" (if i > 0 then "," else "") string_of_type t
    ) tl;
  Format.fprintf fmt ")"
  
and string_of_type fmt = function
  | TTint -> Format.fprintf fmt "int"
  | TTbool -> Format.fprintf fmt "bool"
  | TTstring -> Format.fprintf fmt "str"
  | TTnil | TTuntyped -> assert false
  | TTunit -> Format.fprintf fmt "unit"
  | TTstruct s -> Format.fprintf fmt "%s" s
  | TTtuple tl -> string_of_type_list fmt tl
  | TTpointer t -> Format.fprintf fmt "*%a" string_of_type t

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
     Format.fprintf fmt "%a(%a)" AstUtils.string_of_unop op string_of_texpr texpr
  | TEbinop (op, l, r) ->
     Format.fprintf fmt "@[(%a %a@ %a)@]"
       string_of_texpr l AstUtils.string_of_binop op string_of_texpr r

let rec type_of_expr e =
  match e.Ast.desc with 
  | Ast.Eident str ->
     type_of_string e.loc str
  | Ast.Eunop (Udref, e_t) ->
     let typ = type_of_expr e_t in
     TTpointer typ
  | _ ->
     type_error e.loc
       (Format.asprintf "%a is not a type" AstUtils.string_of_expr e.desc)
    
let rec type_of_ast_typ = function
  | Ast.Tbasic (str, loc) -> type_of_string loc str
  | Ast.Tpointer typ -> TTpointer (type_of_ast_typ typ)

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
     else type_error exp.loc
            (Format.asprintf "cannot take the address of %a"
               AstUtils.string_of_expr exp.desc)
  | Udref, TTnil ->
     type_error exp.loc (Format.asprintf "invalid indirect of nil")
  | _, TTuntyped ->
     assert false
  | (Ast.Unot, (TTint | TTstring | TTnil | TTunit | TTstruct _ | TTtuple _ | TTpointer _))
  | (Ast.Uneg, (TTstring | TTbool | TTnil | TTunit | TTstruct _ |  TTtuple _ | TTpointer _))
  | (Ast.Udref, (TTint | TTstring | TTbool | TTunit | TTstruct _ |TTtuple _)) ->
     type_error exp.loc
       (Format.asprintf "invalid operation: %a %a"
          AstUtils.string_of_unop op string_of_type t_exp.typ)

let type_of_binop = function
  | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv | Ast.Bmod -> TTint
  | Ast.Beq | Ast.Bneq | Ast.Blt | Ast.Ble | Ast.Bgt
  | Ast.Bge | Ast.Band | Ast.Bor -> TTbool

let binop_expected_type = function
  | Badd | Bsub | Bmul | Bdiv | Bmod | Blt | Ble | Bgt | Bge -> Some TTint
  | Beq | Bneq -> None
  | Band | Bor -> Some TTbool
  
let verify_operand_type op loc term = function
  | None, t | Some TTnil, (TTpointer _ as t)  | Some (TTpointer _ as t), TTnil ->
     t
  | Some TTnil, TTnil ->
     type_error loc
       (Format.asprintf "invalid operation: nil %a nil (operator %a not defined on nil)"
          AstUtils.string_of_binop op AstUtils.string_of_binop op)
  | Some exp_typ, typ when typ = exp_typ ->
     typ
  | Some exp_typ, typ ->
     type_error loc (Format.asprintf "cannot convert %a (type %a) to type %a"
                       AstUtils.string_of_expr term string_of_type typ string_of_type exp_typ)

let tdesc_of_select t_str fd =
  match t_str.typ, t_str.tdesc with
  | TTpointer (TTstruct _), _ ->
     TEselect_dref (t_str, fd)
  | TTstruct _, TEunop (Ast.Udref, t_str) -> (* prefer C's "->" over C's "." *)
     TEselect_dref (t_str, fd)
  | TTstruct _, _ ->
     TEselect (t_str, fd)
  | _, _ ->
     assert false

let single_texpr_compatible_types ty_ref ty_act loc f_msg =
  match ty_ref, ty_act with
  | t_r, t_a when t_r = t_a -> ()
  | TTpointer _, TTnil
  | TTuntyped, (TTint | TTstring | TTbool | TTstruct _ | TTpointer _) -> ()
  | TTuntyped, TTnil -> type_error loc (Format.asprintf "use of untyped nil")
  | TTuntyped, _ | _, (TTunit | TTuntyped | TTtuple _) -> assert false
  | e_f, t_a -> type_error loc (f_msg ())
    
let multi_texpr_compatible_types ty_ref (te_act:Asg.texpr) f_msg =
  match ty_ref, te_act.typ with
  | t_r, t_a when t_r = t_a ->
     te_act
  | TTpointer _, TTnil -> (* TTnil's "unification" *)
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
  
let new_var id b_number ?(loc=Utils.dummy_loc) ty =
  { id; b_number; ty; loc }

let underscore ty =
  { id = "_"; b_number = 0; ty; loc = Utils.dummy_loc }

(* unique expression `_` *)
let under_texpr =
  { tdesc = TEident (underscore TTuntyped); typ = TTuntyped;
    is_assignable = true; loc = Utils.dummy_loc }

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
     type_error e.loc "cannot use _ as value"
  | Ast.Eident v ->
     begin
       try
         let tvar = Utils.Smap.find v env in
         TEident tvar, tvar.ty, true
       with Not_found ->
         type_error e.loc (Format.sprintf "undefined %s" v)
     end
  | Ast.Eselect (str, (fd, fd_loc)) ->
     begin
       let t_str = type_expr env str in
       match t_str.typ with
       | TTstruct s | TTpointer (TTstruct s) as t ->
          let { fields; loc = _ } = Utils.Smap.find s !struct_env in
          let fd_type =
            try List.assoc fd fields
            with Not_found ->
              type_error fd_loc
                (Format.asprintf "%a.%s undefined (type %a has no field %s)"
                   AstUtils.string_of_expr str.desc fd string_of_type t fd)
          in
          tdesc_of_select t_str fd, fd_type, t_str.is_assignable
       | TTint | TTstring | TTbool | TTunit | TTtuple _ | TTpointer _ as t ->
          type_error str.loc
            (Format.asprintf "%a.%s undefined (type %a has no field %s)"
               AstUtils.string_of_expr str.desc fd string_of_type t fd)
       | TTnil ->
          type_error str.loc (Format.sprintf "nil pointer dereference")
       | TTuntyped ->
          assert false
     end
  | Ast.Ecall (f, actuals) ->
     begin
       (* check if f has not been overriden by a variable *)
       try
         let var = Utils.Smap.find f env in
         type_error e.loc
           (Format.asprintf "cannot call non-function %s (type %a)"
              f string_of_type var.ty)
       with Not_found ->
         (* check if f has been defined as a function *)
         try
           let { formals; rtype; body = _; loc = _ } = Utils.Smap.find f !func_env in
           let actuals = type_fun_params env f e.loc formals actuals in
           TEcall (f, actuals), rtype, false
         with Not_found ->
           (* check if f is a built-in function *)
           if f = "new" then
             match List.map type_of_expr actuals with
             | [] ->
                type_error e.loc "missing argument to new"
             | [t] ->
                TEnew t, TTpointer t, false
             | t :: _ ->
                type_error e.loc (Format.asprintf "too many arguments to new(%a)"
                                    string_of_type t)
           else
             type_error e.loc (Format.sprintf "undefined %s" f)
     end
  | Ast.Eprint el -> (* Behaviour in Go: this outputs (int, exn) *)
     TEprint (type_expr_list env el), TTunit, false
  | Ast.Eunop (op, e) ->
     let t_e = type_expr env e in
     let typ, is_assignable = type_of_unop e op t_e in
     TEunop (op, t_e), typ, is_assignable
  | Ast.Ebinop (op, l, r) ->
     let t_l = type_expr env l in
     let exp_type = binop_expected_type op in
     let left_exp_type = verify_operand_type op l.loc l.desc (exp_type, t_l.typ) in
     let t_r = type_expr env r in
     let _ = verify_operand_type op r.loc r.desc (Some left_exp_type, t_r.typ) in
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
                    type_error p.loc
                      (Format.asprintf "%a used as value"
                         AstUtils.string_of_expr p.desc)
                 | TTtuple tl ->
                    type_error p.loc
                      (Format.asprintf "multiple-value %a in single-value context"
                         AstUtils.string_of_expr p.desc)
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
          type_error act.loc
            (Format.asprintf "%a used as value" AstUtils.string_of_expr act.desc)
       | _ as t ->
          [t]
     in
     let cmp_act_form = compare (List.length ty_actuals) (List.length ty_formals) in
     if cmp_act_form = 0 then begin
         List.iter2
           (fun t_f t_a ->
             single_texpr_compatible_types t_f t_a act.loc
               (fun _ -> Format.asprintf "cannot use %a value as type %a in argument to %s"
                           string_of_type t_a string_of_type t_f f)
           ) ty_formals ty_actuals;
         match ty_actuals with
         | [t_act] when t_act = TTnil -> (* "unification" of TTnil *)
            [{ te_act with typ = List.hd ty_formals }]
         | _ ->
            [te_act]
       end else
       let msg = if cmp_act_form > 0 then "too many" else "not enough" in
       type_error loc
         (Format.asprintf "%s arguments in call to %s\n\t have %a\n\t want %a"
            msg f string_of_type_list ty_actuals string_of_type_list ty_formals)
  | _  ->
     let cmp_act_form = compare (List.length actuals) (List.length ty_formals) in
     if cmp_act_form = 0 then begin
         List.map2 (fun ty_form act -> 
             let t_act = type_expr env act in
             multi_texpr_compatible_types ty_form t_act
               (Format.sprintf "argument to %s" f)
           ) ty_formals actuals 
       end else
       let msg = if cmp_act_form > 0 then "too many" else "not enough" in
       type_error loc
         (Format.asprintf "%s arguments in call to %s\n\t have %a\n\t want %a"
            msg f string_of_type_list
            (List.map (fun act -> (type_expr env act).typ) actuals)
            string_of_type_list ty_formals)

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
          type_error value.loc
            (Format.asprintf "%a used as value" AstUtils.string_of_expr value.desc)
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
                      string_of_texpr te_value string_of_type t_v
                      string_of_type te_ass.typ
               else fun _ ->
                    Format.asprintf "cannot assign %a to %a (type %a) in multiple assignment"
                      string_of_type t_v string_of_texpr te_ass
                      string_of_type te_ass.typ
             in
             single_texpr_compatible_types te_ass.typ t_v te_value.loc f_msg
           ) to_be_assigned t_values;
         match t_values with
         | [t_val] when t_val = TTnil -> (* "unification" of TTnil *)
            [{ te_value with typ = (List.hd to_be_assigned).typ }]
         | _ ->
            [te_value]
       end else
       type_error loc
         (Format.asprintf "assignment mismatch: %d variable(s) but %a returns %d value(s)"
            l_assigned AstUtils.string_of_expr value.desc l_values)
  | _  ->
     let l_values = List.length values in
     if l_assigned = l_values then begin
         List.map2
           (fun te_ass val_ ->
             let te_val = type_expr env val_ in
             multi_texpr_compatible_types te_ass.typ te_val "assignment"
           ) to_be_assigned values 
       end else
       type_error loc
         (Format.asprintf "assignment mismatch: %d variable(s) but %d value(s)"
            l_assigned l_values)
     
let type_underscores to_be_assigned t_values =
  let t_types =
    match t_values with
    | [{ tdesc; typ = TTtuple tl; is_assignable; loc }] -> tl
    | _ as vs -> List.map (fun t_v -> t_v.typ) vs
  in
  List.map2 (fun t_e t ->
      match t_e.tdesc with
      | TEident tvar when tvar.id = "_" ->
         { t_e with tdesc = TEident { tvar with ty = t }; typ = t }
      | _ ->
         t_e
    ) to_be_assigned t_types
     
let update_env env var_map = function
  | TSdeclare (var_list, values) ->
     List.fold_left (fun env var ->
         if var.id = "_" then env
         else
           try Utils.Smap.add var.id (Utils.Smap.find var.id var_map) env
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
          type_error e.loc
            (Format.asprintf "%a evaluated but not used"
               AstUtils.string_of_expr e.desc)
       | Ecall (f, actuals) ->
          let t_call = type_expr env e in
          let actuals = match t_call.tdesc with | TEcall (_, act) -> act | _ -> assert false
          in
          b_vars, TScall (f, actuals)
       | Eprint el ->
          if not !import_fmt then type_error e.loc "undefined: fmt";
          used_fmt := true;
          b_vars, TSprint (type_expr_list env el)
     end
  | Ast.Iincr e | Idecr e as i_d->
     let string_of_op =
       match i_d with | Iincr _ -> "++" | Idecr _ -> "--" | _ -> assert false
     in
     let t_e = type_expr env e in
     if t_e.is_assignable then
       if t_e.typ = TTint
       then b_vars,
            match i_d with
            | Iincr _ -> TSincr t_e
            | Idecr _ -> TSdecr t_e
            | _ -> assert false
       else type_error e.loc
              (Format.asprintf "invalid operation %a%s (non-numeric type %a)"
                 AstUtils.string_of_expr e.desc string_of_op string_of_type t_e.typ)
     else type_error e.loc
            (Format.asprintf "cannot assign to %a" AstUtils.string_of_expr e.desc)
  | Ast.Iassign (assigned_s, values) ->
     let t_assigned_s = List.map (fun ass ->
                            if ass.Ast.desc = Ast.Eident "_" then under_texpr
                            else
                              let t_ass = type_expr env ass in
                              if not t_ass.is_assignable then
                                type_error ass.loc
                                  (Format.asprintf "cannot assign to %a"
                                     AstUtils.string_of_expr ass.desc);
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
       | [] -> assert false
       | [{ tdesc; typ = TTtuple types; is_assignable; loc }] -> types
       | [{ tdesc; typ = (TTint|TTstring|TTbool|TTstruct _|TTpointer _ as t);
            is_assignable; loc }] -> [t]
       | _ as types -> List.map (fun t_e -> t_e.typ) types
     in
     let rec update_var_map (var_map:Asg.tvar Utils.smap) t_vars = function
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
            let previous_decl = Utils.Smap.find id var_map in
            let line, fst_char, last_char = Utils.position_of_loc previous_decl.loc in
            type_error loc
              (Format.sprintf
                 "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
                 id fst_char last_char line)
          with Not_found ->
            let n_var = new_var id b_number ~loc typ in
            update_var_map (Utils.Smap.add id n_var var_map) (n_var :: t_vars)
              (var_names, types)
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
       type_error cond.loc
         (Format.asprintf "non-bool %a (type %a) used as if condition"
            AstUtils.string_of_expr cond.desc string_of_type t_cond.typ);
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
            match ty with | Some t -> type_of_ast_typ t | None -> assert false
          in
          let rec update_var_map (var_map:Asg.tvar Utils.smap) t_vars = function
            | [] ->
               var_map, List.rev t_vars
            | (id, loc) :: var_names when id = "_" ->
               (* variable "_" is not added to the local environment *)
               update_var_map var_map (underscore typ :: t_vars) var_names
            | (id, loc) :: var_names ->
               (* check name unicity *)
               try
                 let previous_decl = Utils.Smap.find id var_map in
                 let line, fst_char, last_char = Utils.position_of_loc previous_decl.loc in
                 type_error loc
                   (Format.sprintf
                      "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
                      id fst_char last_char line)
               with Not_found ->
                 let n_var = new_var id b_number ~loc typ in
                 update_var_map (Utils.Smap.add id n_var var_map)
                   (n_var :: t_vars) var_names
                 
          in
          let b_vars, t_assigned_s = update_var_map b_vars [] vars in
          b_vars, TSdeclare (t_assigned_s, [])
       | _ -> (* declare-and-assign *)
          let typ =
            match ty with | Some t -> Some (type_of_ast_typ t) | None -> None
          in
          let vars_ph =
            match typ with
            | Some typ -> List.map (expr_placeholder ~typ) vars
            | None -> List.map expr_placeholder vars
          in
          let loc = (List.hd values).loc in
          let t_values = type_assigned_values env loc vars_ph values in
          let types =
            match t_values with
            | [{ tdesc; typ = TTtuple types; is_assignable; loc }] -> types
            | _ as types -> List.map (fun t_e -> t_e.typ) types
          in
          let rec update_var_map (var_map:Asg.tvar Utils.smap) t_vars = function
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
                 let previous_decl = Utils.Smap.find id var_map in
                 let line, fst_char, last_char = Utils.position_of_loc previous_decl.loc in
                 type_error loc
                   (Format.sprintf
                      "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
                      id fst_char last_char line)
               with Not_found ->
                 let n_var = new_var id b_number ~loc typ in
                 update_var_map (Utils.Smap.add id n_var var_map)
                   (n_var :: t_vars) (var_names, types)
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
            Utils.Smap.empty, TSnop, env, b_number
         | Some st ->
            let outer_number = next_bnumber () in
            let out_vars, t_init = type_shstmt env Utils.Smap.empty outer_number st in
            out_vars, t_init, update_env env out_vars t_init, outer_number
       in
       (* condition *)
       let t_cond = type_expr env cond in
       if t_cond.typ <> TTbool then
         type_error cond.loc
           (Format.asprintf "non-bool %a (type %a) used as for condition"
              AstUtils.string_of_expr cond.desc string_of_type t_cond.typ);
       (* post statement & body *)
       let for_number = next_bnumber () in
       let for_vars, for_stmts =
         match post with
         | None -> type_block env body for_number
         | Some st -> type_block env body ~ending_stmt:(Ast.Sexec st) for_number
       in
       let for_block =
         TSfor (t_cond, { vars = for_vars; stmts = for_stmts; number = for_number })
       in
       b_vars,
       if outer_number = b_number then for_block
       else
         TSblock { vars = outer_vars; stmts = t_init :: [for_block]; number = outer_number }
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
  in loop env Utils.Smap.empty stmts ending_stmt [] number

let rec type_vars_of_decl (vars, typ) typ_list msg =
  match vars with
  | [] ->
     typ_list
  | (id, loc) :: vars ->
     try
       let _ = List.assoc id typ_list in
       type_error loc (Format.asprintf "duplicate %s %s" msg id)
     with Not_found ->
       type_vars_of_decl (vars, typ) ((id, typ) :: typ_list) msg

let type_vars_list msg vars =
  List.rev (
      List.fold_left (fun typ_list (vars, typ) ->
          let typ = type_of_ast_typ typ in
          type_vars_of_decl (vars, typ) typ_list msg
        ) [] vars
    )
   
let rec identify_declarations structs functions = function
  | [] ->
     List.rev structs, List.rev functions
  | Ast.Dstruct ((name, loc), fields) :: decls ->
     begin
       try
         let { fields = _; loc = previous_loc } = Utils.Smap.find name !struct_env in
         let line, fst_char, last_char = Utils.position_of_loc previous_loc in
         type_error loc
           (Format.asprintf
              "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
              name fst_char last_char line)
       with Not_found ->
         struct_env := Utils.Smap.add name { fields = []; loc } !struct_env
     end;
     identify_declarations ((name, loc, fields) :: structs) functions decls
    | Ast.Dfunc ((name, loc), args, rtype, body) :: decls ->
       identify_declarations structs ((name, loc, args, rtype, body) :: functions) decls

let type_fun_sign (name, loc, args, rtype, body) =
  try
    let { formals = _; rtype = _; body = _; loc = previous_loc } =
      Utils.Smap.find name !func_env
    in
    let line, fst_char, last_char = Utils.position_of_loc previous_loc in
    type_error loc
      (Format.asprintf
         "%s redeclared in this block\n\t previous declaration in characters %d-%d at line %d"
         name fst_char last_char line)
  with Not_found ->
    (* check arguments *)
    let formals = type_vars_list "argument" args in
    (* check return type *)
    let rtype =
      match List.map type_of_ast_typ rtype with
      | [] -> TTunit
      | [t] -> t
      | _ as ts -> TTtuple ts
    in
    func_env :=
      Utils.Smap.add name { formals; rtype; body = Untyped body; loc } !func_env

let type_struct_fields (name, loc, fields) =
  (* type fields *)
  let fields = type_vars_list "field" fields in
  struct_env := Utils.Smap.add name { fields; loc } !struct_env

let type_fun_body (name, _, _, _, _) =
  let func = Utils.Smap.find name !func_env in
  block_number := 0;
  let b_number = 0 in
  let env = List.fold_left (fun env (id, t) ->
                Utils.Smap.add id (new_var id b_number t) env
              ) Utils.Smap.empty func.formals
  in
  let body = match func.body with Untyped b -> b | Typed _ -> assert false in
  let vars, stmts = type_block env body b_number in
  (* check return *)
  let tbody = Typed { vars; stmts; number = b_number } in
  func_env := Utils.Smap.add name { func with body = tbody } !func_env

let rec scan_texpr env use_queue expr =
  match expr.tdesc with
  | TEint _ | TEstring _ | TEbool _ | TEnil | TEnew _ ->
     env, use_queue
  | TEident tvar when tvar.id = "_" ->
     env, use_queue
  | TEident tvar ->
     begin
       try
         let occ, loc = Utils.Smap.find tvar.id env in
         Utils.Smap.add tvar.id (occ + 1, loc) env, use_queue
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
  List.fold_left (fun (env, queue) exp ->
      scan_texpr env queue exp
    ) (env, use_queue) texpr_list

let rec reduce_queue env new_queue = function
  | [] ->
     env, new_queue
  | id :: queue ->
     let env, added =
       try
         let occ, loc = Utils.Smap.find id env in
         Utils.Smap.add id (occ + 1, loc) env, true
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
       List.filter (fun exp ->
           match exp.tdesc with | TEident _ -> false | _ -> true
         ) to_be_assigned
     in
     let env, use_queue = scan_texpr_list env use_queue used_texprs in
     let env, use_queue = scan_texpr_list env use_queue values in
     env, use_queue, false
  | TSdeclare (vars, values) ->
     let env, use_queue = scan_texpr_list env use_queue values in
     List.fold_left (fun env v ->
         if v.id = "_" then env else Utils.Smap.add v.id (0, v.loc) env
       ) env vars, use_queue, false
  | TSreturn te_actuals ->
     let env, use_queue = scan_texpr_list env use_queue te_actuals in
     begin
       match te_actuals with
       | [te_act] ->
          let act_rtype =
            match te_act.typ with
            | TTtuple tl -> tl
            | _ as t -> [t]
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
                List.fold_left2 (fun (env, queue) exp te_act ->
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
    List.fold_left (fun (env, use_queue, _) st ->
        scan_tstmt loc env use_queue rtype st
      ) (Utils.Smap.empty, [], false) block.stmts in
  Utils.Smap.iter (fun id (occ, loc) ->
      if occ = 0 then type_error loc (Format.sprintf "%s declared and not used" id)
    ) b_env;
  let env, use_queue = reduce_queue env use_queue b_queue in
  env, use_queue, final

let check_fun_return fun_env =
  let check_one_function name { formals; rtype; body; loc } =
    let env =
      List.fold_left (fun env (id, _) ->
          Utils.Smap.add id (0, loc) env
        ) Utils.Smap.empty formals
    in
    let block = match body with | Typed b -> b | Untyped _ -> assert false in
    let exp_rtype =
      match rtype with
      | TTunit -> []
      | TTtuple tl -> tl
      | _ as t -> [t]
    in
    let env, _, final = scan_block loc env [] exp_rtype block in
    if not final && exp_rtype <> [] then type_error loc "missing return at end of function"
  in
  Utils.Smap.iter check_one_function fun_env

let check_recursive_struct struct_env =
  let check_one_struct root ({ fields; loc } as str) =
    let rec dfs_scan curr_struct =
      List.iter
        (function
         | _, TTstruct curr when curr = root -> 
            type_error loc  (Format.asprintf "invalid recursive type %s" root)
         | _, TTstruct curr ->
            dfs_scan (Utils.Smap.find curr struct_env)
         | _ ->
            ()
        ) curr_struct.fields
    in dfs_scan str 
  in
  Utils.Smap.iter check_one_struct struct_env
   
let check_fun_main functions =
  try
    List.iter (fun (name, loc, args, rtype, _) ->
        if name = "main" then
          if args = [] && rtype = [] then raise Found_main
          else type_error loc "func main must have no arguments and no return values"
      ) functions;
    type_error Utils.dummy_loc "function main is undeclared in the main package"
  with Found_main -> ()

let programme file =
  import_fmt := fst file.Ast.import;
  let import_loc = snd file.Ast.import in
  let structs, functions = identify_declarations [] [] file.Ast.decls in
  List.iter type_fun_sign functions;
  List.iter type_struct_fields structs;
  List.iter type_fun_body functions;
  check_fun_return !func_env;
  check_recursive_struct !struct_env;
  check_fun_main functions;
  if !import_fmt <> !used_fmt then
    type_error import_loc "imported and not used: \"fmt\"";
  { structs = !struct_env; functions = !func_env }            


open Ty_ast

exception Typing_error of Ast.loc * string

(* module Smap = Map.Make(String) *)

let struct_env : Ty_ast.struct_ Smap.t ref = ref Smap.empty
let func_env : Ty_ast.func Smap.t ref = ref Smap.empty

let length_of_type = function
  | Tint | Tstring | Tbool | Tnil | Tstruct _ | Tpointer _ ->
     1
  | Ttuple tl -> List.length tl
                                      
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
     if t_exp.left then Tpointer t, false
     else raise (Typing_error (exp.loc, Format.asprintf "cannot take the address of %a"
                                          Utils.string_of_expr exp.desc))
  | Ast.Udref, Tnil ->
     raise (Typing_error (exp.loc, Format.asprintf "invalid indirect of nil"))
  | (Ast.Unot, (Tint | Tstring | Tnil | Tstruct _ | Ttuple _ | Tpointer _))
  | (Ast.Uneg, (Tstring | Tbool | Tnil | Tstruct _ |  Ttuple _ | Tpointer _))
  | (Ast.Udref, (Tint | Tstring | Tbool | Tstruct _ |Ttuple _)) ->
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
    
let loc =
  Lexing.dummy_pos, Lexing.dummy_pos

let rec type_expr env (e:Ast.expr) =
  let tdesc, typ, left = compute_type env e in
  { tdesc; typ; left }
  
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
       try TEident v, (let feats = Smap.find v env in feats.typ), true
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
              TEselect (s, fd), Smap.find fd fields, t_str.left
            with Not_found ->
              raise (Typing_error
                       (fd_loc, Format.asprintf "%a.%s undefined (type %a has no field %s)"
                                   Utils.string_of_expr str.desc fd string_of_type t fd))   
          end
       | Tint | Tstring | Tbool | Ttuple _ | Tpointer _ as t ->
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
  (* | Ast.Eprint el ->
   *    TEprint (check_expr_list env el), Tint, false *)
  | Ast.Eunop (op, e) ->
     let t_e = type_expr env e in
     let typ, left = type_of_unop e op t_e in
     TEunop (op, t_e), typ, left
  | Ast.Ebinop (op, l, r) ->
     let t_l = type_expr env l in
     let exp_type = verify_operand_type op l.loc l.desc (expected_type op, t_l.typ) in
     let t_r = type_expr env r in
     let exp_type = verify_operand_type op r.loc r.desc (Some exp_type, t_r.typ) in
     TEbinop (op, t_l, t_r), exp_type, false

and check_fun_params env f loc formals actuals : Ty_ast.texpr list =
  let t_formals = match formals with | Ttuple l -> l | _ as t -> [t] in
  match actuals with
  | [act] ->
     let t_act = type_expr env act in
     let t_actuals = match t_act.typ with | Ttuple l -> l | _ as t -> [t] in
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
    
let check_expr_list env = function
  | [] ->
     []
  | [p] ->
     [ type_expr env p ]
  | _ as pl ->
     List.map
       (fun p -> let t_p = type_expr env p in
                 match t_p.typ with
                 | Ttuple tl ->
                    (* 0 values *)
                    if tl = [] then raise (Typing_error
                                             (p.loc,
                                              Format.asprintf "%a used as value"
                                                Utils.string_of_expr p.desc))
                    (* >= 2 values *)
                    else raise (Typing_error
                                  (p.loc,
                                   Format.asprintf "multiple-value %a in single-value context"
                                     Utils.string_of_expr p.desc))
                 | _ ->
                    t_p
       ) pl

let rec type_stmt (env:Ty_ast.var Smap.t) = function
  | Ast.Snop ->
     TSnop
  | Ast.Sexec (Ieval e) ->
     begin
       match e.desc with
       | Ecst _ | Eident _ | Eselect _ | Eunop _ | Ebinop _ ->
          raise (Typing_error
                   (e.loc, Format.asprintf "%a evaluated but not used" Utils.string_of_expr e.desc))
       | Ecall (f, actuals) ->
          let t_call = type_expr env e in
          let actuals = match t_call.tdesc with | TEcall (_, act) -> act | _ -> assert false in
          TScall (f, actuals)
     end
  | Ast.Sexec (Iprint el) ->
     TSprint (check_expr_list env el)
  | Ast.Sexec (Iincr e) ->
     let t_e = type_expr env e in
     if t_e.left then
       if t_e.typ = Tint then TSincr t_e
       else raise (Typing_error
                     (e.loc,
                      Format.asprintf "invalid operation %a++ (non-numeric type %a)"
              Utils.string_of_expr e.desc string_of_type t_e.typ))
     else raise (Typing_error
                   (e.loc, Format.asprintf "cannot assign to %a" Utils.string_of_expr e.desc))
  | Ast.Sexec (Idecr e) ->
     let t_e = type_expr env e in
     if t_e.left then
       if t_e.typ = Tint then TSdecr t_e
       else raise (Typing_error
                     (e.loc,
                      Format.asprintf "invalid operation %a-- (non-numeric type %a)"
              Utils.string_of_expr e.desc string_of_type t_e.typ))
     else raise (Typing_error
                   (e.loc, Format.asprintf "cannot assign to %a" Utils.string_of_expr e.desc))
  | Ast.Sexec (Iset (ls, vs)) ->
     assert false (* TODO *)
  | Ast.Sexec (Iassign (vars, values)) ->
     assert false (* TODO *)
  | Ast.Sblock b ->
     assert false (* TODO *)
  | Ast.Sif (cond, bif, belse) ->
     assert false (* TODO *)
  | Ast.Sinit (vars, ty, values) ->
     assert false (* TODO *)
  | Ast.Sreturn (exps) ->
     TSreturn (check_expr_list env exps)
  | Ast.Sfor (init, cond, post, block) ->
     assert false (* TODO *)

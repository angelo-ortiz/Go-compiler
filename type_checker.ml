
open Ty_ast

exception Typing_error of Ast.loc * string

module Smap = Map.Make(String)

(* let struct_env = ref Smap.empty *)
let func_env = Smap.empty

let rec string_of_type fmt = function
  | Tint ->
     Format.fprintf fmt "int"
  | Tbool ->
     Format.fprintf fmt "bool"
  | Tstring ->
     Format.fprintf fmt "str"
  | Tstruct s ->
     Format.fprintf fmt "%s" s
  | Tfunc tl ->
     Format.fprintf fmt "func"
  | Tpointer t ->
     Format.fprintf fmt "*%a" string_of_type t

let type_of_unop loc op typ =
  match op, typ with
  | Ast.Unot, Tbool ->
     Tbool
  | Ast.Uneg, Tint ->
     Tint
  | Ast.Udref, Tpointer t ->
     t
  | Ast.Uaddr, t ->
     Tpointer t
  | (Ast.Unot, (Tint|Tstring|Tstruct _|Tfunc _|Tpointer _))
  | (Ast.Uneg, (Tstring|Tbool|Tstruct _| Tfunc _|Tpointer _))
  | (Ast.Udref, (Tint|Tstring|Tbool|Tstruct _|Tfunc _)) ->
     raise (Typing_error
              (loc, Format.asprintf "invalid operation %s %a"
                      (*Utils.string_of_unop op*) "op" string_of_type typ))

let expected_type = function
  | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv | Ast.Bmod | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge ->
     Some Tint
  | Ast.Beq | Ast.Bneq ->
     None
  | Ast.Band | Ast.Bor ->
     Some Tbool
  
let verify_operand_type loc op typ = function
  | None ->
     typ
  | Some exp_typ when typ = exp_typ ->
     typ
  | Some exp_typ ->
     raise (Typing_error
              (loc, Format.asprintf "cannot convert %a (type %a) to type %a"
                      Utils.string_of_expr op string_of_type typ string_of_type exp_typ))
    
let loc =
  Lexing.dummy_pos, Lexing.dummy_pos

let rec type_expr env e =
  let tdesc, typ = compute_type env e in
  { tdesc; typ }
  
and compute_type env e =
  match e.Ast.desc with
  | Ast.Ecst (Cint n) ->
     TEint n, Tint
  | Ast.Ecst (Cstring s) ->
     TEstring s, Tstring
  | Ast.Ecst (Cbool b) ->
     TEbool b, Tbool
  | Ast.Ecst Cnil ->
     TEnil, Tpointer Tint (* TODO *)
  | Ast.Eident v ->
     begin
       try TEident { id = v; level = 0 (*TODO*); offset = 0 (*TODO*)}, Smap.find v env
       with Not_found -> raise (Typing_error (e.loc, Format.sprintf "undefined %s" v))
     end
  | Ast.Eaccess (str, field) ->
     assert false
  | Ast.Ecall (f, actuals) ->
     begin
       try
         let formals, t, _ = Smap.find f func_env in
         let actuals = List.map (type_expr env) actuals in
         if List.exists2 (<>) formals actuals then raise; (* TODO: exact error location!!! *)
         TEcall (f, List.map (type_expr env) actuals),
         let _, t, _ = Smap.find f func_env in t (* ref *)
       with Not_found -> failwith "" (*TODO*)
     end
  | Ast.Eprint el ->
     TEprint (List.map (type_expr env) el), Tint
  | Ast.Eunop (op, e) ->
     let { tdesc; typ } as te = type_expr env e in
     TEunop (op, te), type_of_unop e.loc op typ
  | Ast.Ebinop (op, l, r) ->
     let t_l = type_expr env l in
     let exp_type = verify_operand_type l.loc l.desc t_l.typ (expected_type op) in
     let t_r = type_expr env r in
     let exp_type = verify_operand_type r.loc r.desc t_r.typ (Some exp_type) in
     TEbinop (op, t_l, t_r), exp_type

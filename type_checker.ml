
open Ty_ast

exception UnificationFailure of Ty_ast.term * Ty_ast.term
exception Typing_error of (Lexing.position * Lexing.position) * string

module QVar = struct
  type t = tvar
  let compare v1 v2 = Pervasives.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

let rec pp fmt = function
  | Tint -> Format.fprintf fmt "int"
  | Tbool -> Format.fprintf fmt "bool"
  | Tstring -> Format.fprintf fmt "str"
  | Tstruct -> Format.fprintf fmt "struct"
  | Tpointer t -> Format.fprintf fmt "*%a" pp t
  | Tvar v -> pp_var fmt v
            
and pp_var fmt {id; def} =
  Format.fprintf fmt "'%d" id;
  match def with
  | None -> ()
  | Some t -> Format.fprintf fmt "[:=%a]" pp t
            
let rec head = function
  | Tvar { id; def = Some t } ->
     head t
  | t ->
     t
  
let rec canon t =
  match head t with
  | Tint | Tbool | Tstring | Tstruct as t ->
     t
  | Tpointer t ->
     Tpointer (canon t)
  | Tvar v as t ->
     assert (v.def = None); t

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))
  
let rec occur v t =
  match head t with
  | Tint | Tbool | Tstring | Tstruct ->
     false
  | Tpointer t ->
     occur v t
  | Tvar w ->
     QVar.equal v w

let rec unify t1 t2 =
  match head t1, head t2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring | Tstruct, Tstruct ->
     ()
  | Tvar v, Tvar w when QVar.equal v w ->
     ()
  | Tvar v, t2 ->
     if occur v t2 then unification_error t1 t2;
     assert (v.def = None);
     v.def <- Some t2
  | t1, Tvar v ->
     unify t2 t1
  | Tpointer t1, Tpointer t2 ->
     unify t1 t2
  | _ -> unification_error t1 t2

let cant_unify ty1 ty2 =
  try let _ = unify ty1 ty2 in false with UnificationFailure _ -> true

let rec free_vars t =
  match head t with
  | Tint | Tbool | Tstring | Tstruct ->
     QVset.empty
  | Tpointer t ->
     free_vars t
  | Tvar v ->
     QVset.singleton v

let empty : Ty_ast.env = { bindings = Smap.empty ; fvars = QVset.empty }

let norm_qvset set =
  QVset.fold (fun v s -> QVset.union s (free_vars (Tvar v))) set QVset.empty
          
let add gen var typ env =
  let fvars_typ = free_vars typ in
  let qvars, fvars =
    if gen then
      let env_fvars = norm_qvset env.fvars in
      (* the free-var set doesn't change since those in `typ` are either
       used in the schema (quantifier) or already in the environment *)
      QVset.diff fvars_typ env_fvars, env_fvars
    else
      QVset.empty, QVset.union env.fvars fvars_typ      
  in
  { bindings = Smap.add var { qvars ; term = typ } env.bindings; fvars }

let find v env =
  let scheme = Smap.find v env.bindings in
  let fresh_vars =
    QVset.fold (fun v m -> QVmap.add v (Tvar (QVar.create ())) m)
      scheme.qvars QVmap.empty in
  let rec subs t =
    match head t with
    | Tint | Tbool | Tstring | Tstruct as t ->
       t
    | Tpointer t ->
       Tpointer (subs t)
    | Tvar w as t ->
       try QVmap.find w fresh_vars
       with Not_found -> t
  in subs scheme.term

let rec type_expr env = function
  | _ -> Tint
  (* | Var v ->
   *    find v env
   * | Const _ ->
   *    Tint
   * | Op "+" ->
   *    Tarrow (Tproduct (Tint, Tint), Tint)
   * | Op o ->
   *    failwith ("undefined operator " ^ o)
   * | Fun (x, body) ->
   *    let x_typ = Tvar (V.create ()) in
   *    let env = add false x x_typ env in
   *    let body_typ = w env body in
   *    Tarrow (x_typ, body_typ)
   * | App (f, x) ->
   *    let f_typ = w env f in
   *    let x_typ = w env x in
   *    let ret_typ = Tvar (V.create ()) in
   *    unify f_typ (Tarrow (x_typ, ret_typ));
   *    ret_typ
   * | Pair (l, r) ->
   *    Tproduct (w env l, w env r)
   * | Let (x, e1, e2) ->
   *    let e1_typ = w env e1 in
   *    w (add true x e1_typ env) e2 *)

let typeof e = canon (w empty e)

(* 1 : int *)
let () = assert (typeof (Const 1) = Tint)

(* fun x -> x : 'a -> 'a *)
let () = assert (match typeof (Fun ("x", Var "x")) with
  | Tarrow (Tvar v1, Tvar v2) -> V.equal v1 v2
  | _ -> false)

(* fun x -> x+1 : int -> int *)
let () = assert (typeof (Fun ("x", App (Op "+", Pair (Var "x", Const 1))))
                 = Tarrow (Tint, Tint))

(* fun x -> x+x : int -> int *)
let () = assert (typeof (Fun ("x", App (Op "+", Pair (Var "x", Var "x"))))
                 = Tarrow (Tint, Tint))

(* let x = 1 in x+x : int *)
let () =
  assert (typeof (Let ("x", Const 1, App (Op "+", Pair (Var "x", Var "x"))))
          = Tint)

(* let id = fun x -> x in id 1 *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"), App (Var "id", Const 1)))
          = Tint)

(* let id = fun x -> x in id id 1 *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"),
		       App (App (Var "id", Var "id"), Const 1)))
          = Tint)

(* let id = fun x -> x in (id 1, id (1,2)) : int * (int * int) *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"),
		       Pair (App (Var "id", Const 1),
			     App (Var "id", Pair (Const 1, Const 2)))))
          = Tproduct (Tint, Tproduct (Tint, Tint)))

(* app = fun f x -> let y = f x in y : ('a -> 'b) -> 'a -> 'b *)
let () =
  let ty =
    typeof (Fun ("f", Fun ("x", Let ("y", App (Var "f", Var "x"), Var "y"))))
  in
  assert (match ty with
    | Tarrow (Tarrow (Tvar v1, Tvar v2), Tarrow (Tvar v3, Tvar v4)) ->
        V.equal v1 v3 && V.equal v2 v4
    | _ -> false)
                 
let cant_type e =
  try let _ = typeof e in false with UnificationFailure _ -> true


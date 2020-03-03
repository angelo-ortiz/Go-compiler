
open Asg
open Istree

exception Found_offset of int
   
let struct_env = ref Asg.Smap.empty

let rec _length_of_type fs = function
  | TTint | TTbool | TTstring | TTpointer _ ->
     1
  | TTstruct s ->
     fs s
  | TTunit ->
     0
  | TTtuple tl ->
     List.fold_left (fun len t -> len + _length_of_type fs t) 0 tl
  | TTnil ->
     1 (* TODO: put 0 and check if the untyped nil escapes the type analysis *)
  | TTuntyped ->
     assert false

let length_of_struct =
  let h = Hashtbl.create 16 in
  let rec f s =
    try Hashtbl.find h s
    with Not_found ->
      let str = Smap.find s !struct_env in
      let len = List.fold_left (fun len (_, ty) -> len + _length_of_type f ty) 0 str.fields
      in
      Hashtbl.add h s len;
      len
  in
  f

(* the number of 8-byte blocks *)
let length_of_type = _length_of_type length_of_struct

let struct_of_texpr = function
  | Asg.TTstruct str | Asg.TTpointer (Asg.TTstruct str) ->
     str
  | _ ->
     assert false
    
let field_offset str fd =
  let str = Asg.Smap.find str !struct_env in
  try
    let _ = 
      List.fold_left
        (fun ofs (fd', ty) ->
          if fd' = fd then raise (Found_offset ofs); ofs + length_of_type ty) 0 str.fields
    in assert false
  with Found_offset ofs -> ofs
     
(* assert variable-name unicity*)
let prepend_bnumber tvar =
  Format.sprintf "%d_%s" tvar.b_number tvar.id

let expr_of_primitive e int_not_bool =
  { length = 1; desc = e; typ = if int_not_bool then TTint else TTbool }

let default_value typ =
  let rec loop (acc, size) typ =
    match typ with 
    | TTint ->
       let length = length_of_type typ in
       { length; desc = IEint 0l; typ } :: acc, size + length
    | TTstring ->
       let length = length_of_type typ in
       { length; desc = IEstring ""; typ } :: acc, size + length
    | TTbool ->
       let length = length_of_type typ in
       { length; desc = IEbool false; typ } :: acc, size + length
    | TTstruct str ->
       let str = Asg.Smap.find str !struct_env in
       List.fold_left loop (acc, size) (List.map snd str.fields)
    | TTpointer _ ->
       let length = length_of_type typ in
       { length; desc = IEnil; typ }:: acc, size + length
    | TTnil | TTunit | TTuntyped | TTtuple _ ->
       assert false
  in
  match loop ([], 0) typ with
  | [e], _ ->
     e
  | _ as l, length -> (* l is reversed *)
     { length; desc = IElist l; typ }
    
let rec mk_add e1 e2 =
  match e1.desc, e2.desc with
  | IEint n1, IEint n2 ->
     IEint (Int32.add n1 n2)
  | e, IEint 0l | IEint 0l, e ->
     e
  | IEunop (Maddi n1, e), IEint n2 | IEint n2, IEunop (Maddi n1, e) ->
     mk_add (expr_of_primitive (IEint (Int32.add n1 n2)) true) e
  | _, IEint n ->
     IEunop (Maddi n, e1)
  | IEint n, _ ->
     IEunop (Maddi n, e2)
  | _ ->
     IEbinop (Madd, e1, e2)
    
let rec mk_neg e =
  match e.desc with
  | IEint n ->
     IEint (Int32.neg n)
  | IEunop (Maddi n, e) ->
     IEunop (Maddi (Int32.neg n), expr_of_primitive (mk_neg e) true)
  | _ ->
     IEunop (Mneg, e)
    
and mk_sub e1 e2 =
  match e1.desc, e2.desc with
  | IEint n1, IEint n2 ->
     IEint (Int32.sub n1 n2)
  | e, IEint 0l ->
     e
  | IEint 0l, _ ->
     mk_neg e2
  | IEunop (Maddi n1, e), IEint n2 ->
     mk_sub e (expr_of_primitive (IEint (Int32.sub n2 n1)) true)
  | IEint n2, IEunop (Maddi n1, e) ->
     mk_sub (expr_of_primitive (IEint (Int32.sub n2 n1)) true) e
  | _, IEint n ->
     IEunop (Maddi (Int32.neg n), e1)
  | IEint n, _ ->
     IEunop (Maddi n, expr_of_primitive (mk_neg e2) true)
  | _ ->
     IEbinop (Msub, e1, e2)

let rec mk_mul e1 e2 =
  match e1.desc, e2.desc with
  | IEint n1, IEint n2 ->
     IEint (Int32.mul n1 n2)
  | IEunop (Mimuli n1, e), IEint n2 | IEint n2, IEunop (Mimuli n1, e) ->
     mk_mul (expr_of_primitive (IEint (Int32.mul n1 n2)) true) e
  | e, IEint 1l | IEint 1l, e ->
     e
  | _, IEint -1l ->
     IEunop (Mneg, e1)
  | IEint -1l, _ ->
     IEunop (Mneg, e2)
  | _, IEint n ->
     IEunop (Mimuli n, e1)
  | IEint n, _ ->
     IEunop (Mimuli n, e2)
  | _ ->
     IEbinop (Mimul, e1, e2)

let rec mk_div e1 e2 =
  match e1.desc, e2.desc with
  | IEint n1, IEint n2 when n2 = 0l -> (* it will be a running-time error *)
     IEunop (Midivil n2, e1)
  | IEint n1, IEint n2 ->
     IEint (Int32.div n1 n2)
  | e, IEint 1l ->
     e
  | IEunop (Midivil n1, e), IEint n2 ->
     mk_div e (expr_of_primitive (IEint (Int32.mul n1 n2)) true)
  | _, IEint n ->
     IEunop (Midivil n, e1)
  | IEint n, _ ->
     IEunop (Midivir n, e2)
  | _ ->
     IEbinop (Midiv, e1, e2)

let rec mk_mod e1 e2 =
  match e1.desc, e2.desc with
  | IEint n1, IEint n2 when n2 = 0l -> (* it will be a running-time error *)
     IEunop (Mmodil n2, e1)
  | IEint n1, IEint n2 ->
     IEint (Int32.rem n1 n2)
  | _, IEint n ->
     IEunop (Mmodil n, e1)
  | IEint n, _ ->
     IEunop (Mmodir n, e2)
  | _ ->
     IEbinop (Mmod, e1, e2)

let rec mk_not e =
  match e.desc with
  | IEbool b ->
     IEbool (not b)
  | IEunop (Msetei n, e) ->
     IEunop (Msetnei n, e)
  | IEunop (Msetnei n, e) ->
     IEunop (Msetei n, e)
  | IEunop (Msetli n, e) ->
     IEunop (Msetgei n, e)
  | IEunop (Msetlei n, e) ->
     IEunop (Msetgi n, e)
  | IEunop (Msetgi n, e) ->
     IEunop (Msetlei n, e)
  | IEunop (Msetgei n, e) ->
     IEunop (Msetli n, e)
  | IEbinop (Msete, l, r) ->
     IEbinop (Msetne, l, r)
  | IEbinop (Msetne, l, r) ->
     IEbinop (Msete, l, r)
  | IEbinop (Msetl, l, r) ->
     IEbinop (Msetge, l, r)
  | IEbinop (Msetle, l, r) ->
     IEbinop (Msetg, l, r)
  | IEbinop (Msetg, l, r) ->
     IEbinop (Msetle, l, r)
  | IEbinop (Msetge, l, r) ->
     IEbinop (Msetl, l, r) 
  | IEand (l, r) ->
     IEor (expr_of_primitive (mk_not l) false, expr_of_primitive (mk_not r) false)
  | IEor (l, r) ->
     IEand (expr_of_primitive (mk_not l) false, expr_of_primitive (mk_not r) false)
  | _ ->
     IEunop (Mnot, e)

let mk_eq e1 e2 =
  match e1.desc, e2.desc with
  | IEbool b1, IEbool b2 ->
     IEbool (b1 = b2)
  | IEint n1, IEint n2 ->
     IEbool (n1 = n2)
  | IEstring s1, IEstring s2 ->
     IEbool (s1 = s2)
  | IEaccess v1, IEaccess v2 when v1 = v2 ->
     IEbool true
  | _, IEint n ->
     IEunop (Msetei n, e1)
  | IEint n, _ ->
     IEunop (Msetei n, e2)
  | _ ->
     IEbinop (Msete, e1, e2)
    
let mk_lt e1 e2 =
  match e1.desc, e2.desc with
  | IEint n1, IEint n2 ->
     IEbool (n1 < n2)
  | IEaccess v1, IEaccess v2 when v1 = v2 ->
     IEbool false
  | _, IEint n ->
     IEunop (Msetli n, e1)
  | IEint n, _ ->
     IEunop (Msetgi n, e2)
  | _ ->
     IEbinop (Msetl, e1, e2)
    
let mk_le e1 e2 =
  match e1.desc, e2.desc with
  | IEint n1, IEint n2 ->
     IEbool (n1 <= n2)
  | IEaccess v1, IEaccess v2 when v1 = v2 ->
     IEbool true
  | _, IEint n ->
     IEunop (Msetlei n, e1)
  | IEint n, _ ->
     IEunop (Msetgei n, e2)
  | _ ->
     IEbinop (Msetle, e1, e2)

let rec expr e =
  let length, desc, typ = expr_desc e in
  { length; desc; typ }

and expr_desc e =
  match e.tdesc with
  | TEint n ->
     length_of_type e.typ, IEint (Int64.to_int32 n), e.typ
  | TEstring str ->
     length_of_type e.typ, IEstring str, e.typ
  | TEbool b ->
     length_of_type e.typ, IEbool b, e.typ
  | TEnil ->
     length_of_type e.typ, IEnil, e.typ
  | TEnew ty ->
     length_of_type e.typ, IEmalloc (Int32.of_int (Utils.word_size * length_of_type ty)),
     TTpointer ty
  | TEident tvar when tvar.id = "_" ->
     length_of_type e.typ, IEaccess "_", e.typ
  | TEident tvar ->
     length_of_type e.typ, IEaccess (prepend_bnumber tvar), e.typ
  | TEselect (str, fd) ->
     length_of_type e.typ,
     IEselect (expr str, field_offset (struct_of_texpr str.typ) fd), e.typ
  | TEselect_dref (str, fd) ->
     length_of_type e.typ,
     IEload (expr str, Utils.word_size * field_offset (struct_of_texpr str.typ) fd), e.typ
  | TEcall (f, actuals) ->
     length_of_type e.typ, IEcall (f, List.map expr actuals), e.typ
  | TEprint es -> (* print only exists as a statement *)
     assert false
  | TEunop (Ast.Unot, e') ->
     length_of_type e.typ, mk_not (expr e'), e.typ
  | TEunop (Ast.Uneg, e') ->
     length_of_type e.typ, mk_neg (expr e'), e.typ
  | TEunop (Ast.Udref, { tdesc = TEunop (Ast.Uaddr, e); typ; is_assignable; loc }) ->
     expr_desc e
  | TEunop (Ast.Udref, e') ->
     length_of_type e.typ, IEload (expr e', 0), e.typ
  | TEunop (Ast.Uaddr, { tdesc = TEunop (Ast.Udref, e); typ; is_assignable; loc }) ->
     expr_desc e
  | TEunop (Ast.Uaddr, e') ->
     length_of_type e.typ, IEaddr (expr e'), e.typ
  | TEbinop (Ast.Badd, l, r) ->
     length_of_type e.typ, mk_add (expr l) (expr r), e.typ
  | TEbinop (Ast.Bsub, l, r) ->
     length_of_type e.typ, mk_sub (expr l) (expr r), e.typ
  | TEbinop (Ast.Bmul, l, r) ->
     length_of_type e.typ, mk_mul (expr l) (expr r), e.typ
  | TEbinop (Ast.Bdiv, l, r) ->
     length_of_type e.typ, mk_div (expr l) (expr r), e.typ
  | TEbinop (Ast.Bmod, l, r) ->
     length_of_type e.typ, mk_mod (expr l) (expr r), e.typ
  | TEbinop (Ast.Beq, l, r) ->
     length_of_type e.typ, mk_eq (expr l) (expr r), e.typ
  | TEbinop (Ast.Bneq, l, r) ->
     length_of_type e.typ, mk_not (expr_of_primitive (mk_eq (expr l) (expr r)) false), e.typ
  | TEbinop (Ast.Blt, l, r) ->
     length_of_type e.typ, mk_lt (expr l) (expr r), e.typ
  | TEbinop (Ast.Ble, l, r) ->
     length_of_type e.typ, mk_le (expr l) (expr r), e.typ
  | TEbinop (Ast.Bgt, l, r) ->
     length_of_type e.typ, mk_not (expr_of_primitive (mk_le (expr l) (expr r)) false), e.typ
  | TEbinop (Ast.Bge, l, r) ->
     length_of_type e.typ, mk_not (expr_of_primitive (mk_lt (expr l) (expr r)) false), e.typ
  | TEbinop (Ast.Band, l, r) ->
     length_of_type e.typ, IEand (expr l, expr r), e.typ
  | TEbinop (Ast.Bor, l, r) ->
     length_of_type e.typ, IEor (expr l, expr r), e.typ

let assign_desc = function
  | IEaccess v ->
     Avar v
  | IEselect (str, fd) ->
     Afield (str, fd)
  | IEload (str, n) ->
     Adref (str, n)
  | IEint _ | IEstring _ | IEbool _ | IEnil | IEmalloc _
  | IEcall _ |IEaddr _ | IEunop _ | IEbinop _ | IEand _ | IEor _ | IElist _ ->
     assert false

let assign e =
  let { length; desc } = expr e in
  let assignee = assign_desc desc in
  { length; assignee }

let rec stmt locals body = function
  | TSnop ->
     locals, body
  | TScall (f, actuals) ->
     locals, IScall (f, List.map expr actuals) :: body
  | TSprint es when es = [] ->
     (* no expressions to print *)
     locals, body
  | TSprint es ->
     locals, ISprint (List.map expr es) :: body
  | TSincr e ->
     locals, ISexpr (expr_of_primitive (IEunop (Minc, expr e)) true) :: body
  | TSdecr e ->
     locals, ISexpr (expr_of_primitive (IEunop (Mdec, expr e)) true) :: body
  | TSblock b ->
     block locals body b
  | TSif (cond, bif, belse) ->
     let locals, b_if = block locals [] bif in
     let locals, b_else = block locals [] belse in
     locals, ISif (expr cond, List.rev b_if, List.rev b_else) :: body
  | TSassign (assigned_s, values) ->
     locals, ISassign (List.map assign assigned_s, List.map expr values) :: body
  | TSdeclare (vars, values) ->
     if values = [] then
       locals, List.fold_right (fun v acc ->
                   if v.id = "_" then acc
                   else begin
                       let length = length_of_type v.ty in
                       let assignee = Avar (prepend_bnumber v) in
                       ISassign ([{ length; assignee }], [default_value v.ty]) :: acc
                     end
                 ) vars body
     else
       locals,
       ISassign (
           List.map (fun v ->
               let length = length_of_type v.ty in
               let assignee = Avar (if v.id = "_" then "_" else  prepend_bnumber v) in
               { length; assignee }
             ) vars,
           List.map expr values
         ) :: body
  | TSreturn es ->
     locals, ISreturn (List.map expr es) :: body
  | TSfor (cond, bfor) ->
     let locals, b_for = block locals [] bfor in
     locals, ISfor (expr cond, List.rev b_for) :: body
    
and block locals body b =
  let locals = Smap.fold (fun id tvar vs ->
                   (prepend_bnumber tvar, length_of_type tvar.ty) :: vs
                 ) b.vars locals
  in
  let locals, body =
    List.fold_left (fun (vs, st_s) st -> stmt vs st_s st) (locals, body) b.stmts
  in
  locals, body

let funct (f:Asg.tfundef) =
  let result_length = function
    | TTunit ->
       []
    | TTtuple tl ->
       List.map length_of_type tl
    | _ as t ->
       [ length_of_type t ]
  in
  (* local vars at block 0 cannot have the same name as a formal parameter *)
  let formals = List.map (fun (id, ty) -> "0_" ^ id, length_of_type ty) f.formals in
  let locals, body = block [] [] (match f.body with | Typed b -> b | Untyped _ -> assert false) in
  { formals; result = result_length f.rtype; locals = List.rev locals; body = List.rev body }
  
let programme (p:Asg.tprogramme) =
  struct_env := p.structs;
  { structs = Asg.Smap.map (fun str -> str.fields) p.structs;
    functions = Smap.map funct p.functions }

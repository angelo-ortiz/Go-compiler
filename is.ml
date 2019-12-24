
open Asg
open Istree

(* x86_64: 64b *)
let word_size = 8

(* assert variable-name unicity*)
let prepend_bnumber tvar =
  Format.sprintf "%d_%s" tvar.b_number tvar.id
   
let rec mk_add e1 e2 =
  match e1, e2 with
  | IEint n1, IEint n2 ->
     IEint (Int64.add n1 n2)
  | e, IEint 0L | IEint 0L, e ->
     e
  | IEunop (Maddi n1, e), IEint n2 | IEint n2, IEunop (Maddi n1, e) ->
     mk_add (IEint (Int64.add n1 n2)) e
  | e, IEint n | IEint n, e ->
     IEunop (Maddi n, e)
  | _ ->
     IEbinop (Madd, e1, e2)
    
let rec mk_neg = function
  | IEint n ->
     IEint (Int64.neg n)
  | IEunop (Maddi n, e) ->
     IEunop (Maddi (Int64.neg n), mk_neg e)
  | _ as e ->
     IEunop (Mneg, e)
    
and  mk_sub e1 e2 =
  match e1, e2 with
  | IEint n1, IEint n2 ->
     IEint (Int64.sub n1 n2)
  | e, IEint 0L ->
     e
  | IEint 0L, e ->
     mk_neg e
  | IEunop (Maddi n1, e), IEint n2 ->
     mk_sub e (IEint (Int64.sub n2 n1)) 
  | IEint n2, IEunop (Maddi n1, e) ->
     mk_sub (IEint (Int64.sub n2 n1)) e
  | e, IEint n ->
     IEunop (Maddi (Int64.neg n), e)
  | IEint n, e ->
     IEunop (Maddi n, mk_neg e)
  | _ ->
     IEbinop (Msub, e1, e2)

let rec mk_mul e1 e2 =
  match e1, e2 with
  | IEint n1, IEint n2 ->
     IEint (Int64.mul n1 n2)
  | IEunop (Mimuli n1, e), IEint n2 | IEint n2, IEunop (Mimuli n1, e) ->
     mk_mul (IEint (Int64.mul n1 n2)) e
  | e, IEint n | IEint n, e ->
     IEunop (Mimuli n, e)
  | _ ->
     IEbinop (Mimul, e1, e2)

let rec mk_div e1 e2 =
  match e1, e2 with
  | IEint n1, IEint n2 when n2 = 0L ->
     Utils.optimiser_error Utils.dummy_loc "division by zero"
  | IEint n1, IEint n2 ->
     IEint (Int64.div n1 n2)
  | e, IEint 1L ->
     e
  | IEunop (Midivil n1, e), IEint n2 ->
     mk_div e (IEint (Int64.mul n1 n2))
  | e, IEint n ->
     IEunop (Midivil n, e)
  | IEint n, e ->
     IEunop (Midivir n, e)
  | _ ->
     IEbinop (Midiv, e1, e2)

let rec mk_mod e1 e2 =
  match e1, e2 with
  | IEint n1, IEint n2 when n2 = 0L ->
     Utils.optimiser_error Utils.dummy_loc "division by zero"
  | IEint n1, IEint n2 ->
     IEint (Int64.rem n1 n2)
  | e, IEint n ->
     IEunop (Mmodil n, e)
  | IEint n, e ->
     IEunop (Mmodir n, e)
  | _ ->
     IEbinop (Mmod, e1, e2)

let rec mk_not = function
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
  | IEunop (Mjz, e) ->
     IEunop (Mjnz, e)
  | IEunop (Mjnz, e) ->
     IEunop (Mjz, e)
  | IEunop (Mjei n, e) ->
     IEunop (Mjnei n, e)
  | IEunop (Mjnei n, e) ->
     IEunop (Mjei n, e)
  | IEunop (Mjgi n, e) ->
     IEunop (Mjlei n, e)
  | IEunop (Mjgei n, e) ->
     IEunop (Mjli n, e)
  | IEunop (Mjli n, e) ->
     IEunop (Mjgei n, e)
  | IEunop (Mjlei n, e) ->
     IEunop (Mjgi n, e)
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
  | IEbinop (Mje, l, r) ->
     IEbinop (Mjne, l, r)
  | IEbinop (Mjne, l, r) ->
     IEbinop (Mje, l, r)
  | IEbinop (Mjl, l, r) ->
     IEbinop (Mjge, l, r)
  | IEbinop (Mjle, l, r) ->
     IEbinop (Mjg, l, r)
  | IEbinop (Mjg, l, r) ->
     IEbinop (Mjle, l, r)
  | IEbinop (Mjge, l, r) ->
     IEbinop (Mjl, l, r)
  | IEand (l, r) ->
     IEor (mk_not l, mk_not r)
  | IEor (l, r) ->
     IEand (mk_not l, mk_not r)
  | _ as e ->
     IEunop (Mnot, e)

let mk_eq ?(cond=false) e1 e2 =
  match e1, e2 with
  | IEbool b1, IEbool b2 ->
     IEbool (b1 = b2)
  | IEint n1, IEint n2 ->
     IEbool (n1 = n2)
  | IEstring s1, IEstring s2 ->
     IEbool (s1 = s2)
  | IEaccess v1, IEaccess v2 when v1 = v2 ->
     IEbool true
  | IEint n, e | e, IEint n ->
     if cond then
       if n = 0L then IEunop (Mjz, e)
       else IEunop (Mjei n, e)
     else IEunop (Msetei n, e)
  | _ ->
     let op = if cond then Mje else Msete in
     IEbinop (op, e1, e2)
    
let mk_lt ?(cond=false) e1 e2 =
  match e1, e2 with
  | IEint n1, IEint n2 ->
     IEbool (n1 < n2)
  | IEaccess v1, IEaccess v2 when v1 = v2 ->
     IEbool false
  | IEint n, e ->
     let op = if cond then Mjgi n else Msetgi n in
     IEunop (op, e)
  | e, IEint n ->
     let op = if cond then Mjli n else Msetli n in
     IEunop (op, e)
  | _ ->
     let op = if cond then Mjl else Msetl in
     IEbinop (op, e1, e2)
    
let mk_le ?(cond=false) e1 e2 =
  match e1, e2 with
  | IEint n1, IEint n2 ->
     IEbool (n1 <= n2)
  | IEaccess v1, IEaccess v2 when v1 = v2 ->
     IEbool true
  | IEint n, e ->
     let op = if cond then Mjgei n else Msetgei n in
     IEunop (op, e)
  | e, IEint n ->
     let op = if cond then Mjlei n else Msetlei n in
     IEunop (op, e)
  | _ ->
     let op = if cond then Mjle else Msetle in
     IEbinop (op, e1, e2)
    
let rec expr e =
  match e.tdesc with
  | TEint n ->
     IEint n
  | TEstring str ->
     IEstring str
  | TEbool b ->
     IEbool b
  | TEnil ->
     IEnil
  | TEnew ty ->
     IEnew ty
  | TEident tvar ->
     IEaccess (prepend_bnumber tvar)
  | TEselect (str, n) ->
     IEload (expr str, n * word_size)
  | TEcall (f, actuals) ->
     IEcall (f, List.map expr actuals)
  | TEprint es ->
     IEprint (List.map expr es)
  | TEunop (Ast.Unot, e) ->
     mk_not (expr e)
  | TEunop (Ast.Uneg, e) ->
     mk_neg (expr e)
  | TEunop (Ast.Udref, e) ->
     IEunop (Mdref, expr e) (* TODO *)
  | TEunop (Ast.Uaddr, e) ->
     IEunop (Maddr, expr e) (* TODO *)
  | TEbinop (Ast.Badd, l, r) ->
     mk_add (expr l) (expr r)
  | TEbinop (Ast.Bsub, l, r) ->
     mk_sub (expr l) (expr r)
  | TEbinop (Ast.Bmul, l, r) ->
     mk_mul (expr l) (expr r)
  | TEbinop (Ast.Bdiv, l, r) ->
     mk_div (expr l) (expr r)
  | TEbinop (Ast.Bmod, l, r) ->
     mk_mod (expr l) (expr r)
  | TEbinop (Ast.Beq, l, r) ->
     mk_eq (expr l) (expr r)
  | TEbinop (Ast.Bneq, l, r) ->
     mk_not (mk_eq (expr l) (expr r))
  | TEbinop (Ast.Blt, l, r) ->
     mk_lt (expr l) (expr r)
  | TEbinop (Ast.Ble, l, r) ->
     mk_le (expr l) (expr r)
  | TEbinop (Ast.Bgt, l, r) ->
     mk_not (mk_le (expr l) (expr r))
  | TEbinop (Ast.Bge, l, r) ->
     mk_not (mk_lt (expr l) (expr r))
  | TEbinop (Ast.Band, l, r) ->
     IEand (expr l, expr r)
  | TEbinop (Ast.Bor, l, r) ->
     IEor (expr l, expr r)

let rec cond_expr e =
  match e.tdesc with
  | TEbool b ->
     IEbool b
  | TEident tvar ->
     IEunop (Mjnz, IEaccess (prepend_bnumber tvar))
  | TEselect (str, n) ->
     IEunop (Mjnz, IEload (expr str, n * word_size))
  | TEcall (f, actuals) ->
     IEunop (Mjnz, IEcall (f, List.map expr actuals))
  | TEunop (Ast.Unot, e) ->
     mk_not (cond_expr e)
  | TEbinop (Ast.Beq, l, r) ->
     mk_eq ~cond:true (expr l) (expr r)
  | TEbinop (Ast.Bneq, l, r) ->
     mk_not (mk_eq ~cond:true (expr l) (expr r))
  | TEbinop (Ast.Blt, l, r) ->
     mk_lt ~cond:true (expr l) (expr r)
  | TEbinop (Ast.Ble, l, r) ->
     mk_le ~cond:true (expr l) (expr r)
  | TEbinop (Ast.Bgt, l, r) ->
     mk_not (mk_le ~cond:true (expr l) (expr r))
  | TEbinop (Ast.Bge, l, r) ->
     mk_not (mk_lt ~cond:true (expr l) (expr r))
  | TEbinop (Ast.Band, l, r) ->
     IEand (expr l, expr r)
  | TEbinop (Ast.Bor, l, r) ->
     IEor (expr l, expr r)
  | _ ->
     assert false
    
let rec stmt l_vars l_stmts = function
  | TSnop ->
     l_vars, l_stmts
  | TScall (f, actuals) ->
     l_vars, IScall (f, List.map expr actuals) :: l_stmts
  | TSprint es ->
     l_vars, ISprint (List.map expr es) :: l_stmts
  | TSincr e ->
     l_vars, ISexpr (IEunop (Minc, expr e)) :: l_stmts
  | TSdecr e ->
     l_vars, ISexpr (IEunop (Mdec, expr e)) :: l_stmts
  | TSblock b ->
     block l_vars l_stmts b
  | TSif (cond, bif, belse) ->
     let l_vars, b_if = block l_vars [] bif in
     let l_vars, b_else = block l_vars [] belse in
     l_vars, ISif (cond_expr cond, List.rev b_if, List.rev b_else) :: l_stmts
  | TSassign (assigned_s, values) ->
     l_vars, ISassign (List.map expr assigned_s, List.map expr values) :: l_stmts
  | TSdeclare (vars, values) ->
     l_vars,
     ISassign
       (List.map (fun v -> IEaccess (prepend_bnumber v)) vars, List.map expr values) :: l_stmts
  | TSreturn es ->
     l_vars, ISreturn (List.map expr es) :: l_stmts
  | TSfor (cond, body) ->
     let l_vars, b_for = block l_vars [] body in
     l_vars, ISfor (cond_expr cond, b_for) :: l_stmts
    
and block l_vars l_stmts b =
  let l_vars = Smap.fold (fun id tvar vs -> (prepend_bnumber tvar) :: vs) b.vars l_vars in
  let l_vars, l_stmts =
    List.fold_left (fun (l_vs, l_sts) st -> stmt l_vs l_sts st) (l_vars, l_stmts) b.stmts
  in
  List.rev l_vars, List.rev l_stmts
  
let function_ (f:Asg.decl_fun) =
  let locals, body = block [] [] (match f.body with | Typed b -> b | Untyped _ -> assert false) in 
  { formals = List.map fst f.formals; return = Utils.size_of_type f.rtype; locals; body }
  
let file (file:Asg.tfile) =
  let structs = Smap.map (fun str -> List.length str.fields) file.structs in
  let functions = Smap.map function_ file.functions in
  { structs; functions }

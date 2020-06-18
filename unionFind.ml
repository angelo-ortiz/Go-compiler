
type 'a element = {
    mutable link : 'a link;
  }
                
and 'a link =
  | Repr of 'a info      (* The element is a class representative *)
  | Child of 'a element  (* The element is a child of its 'content' *)

and 'a info = {
    mutable rank : int;
    mutable contents : 'a;
  }
    
let fresh contents =
  { link = Repr { rank = 0; contents } }

let info_of_repr elt =
  match elt.link with
  | Repr i -> i
  | Child _ -> assert false
    
(* When equally ranked, make [rep2] a child of [rep1] *)
let link rep1 rep2 =
  let info1 = info_of_repr rep1 in
  let info2 = info_of_repr rep2 in
  if info1.rank < info2.rank then
    rep1.link <- Child rep2
  else begin
      rep2.link <- Child rep1;
      info1.contents <- info2.contents;
      if info1.rank = info2.rank then
        info1.rank <- succ info1.rank
  end
  
let rec find elt =
  match elt.link with
  | Repr _ ->
     elt
  | Child p ->
     let new_p = find p in
     elt.link <- Child new_p;
     new_p

let union elt1 elt2 =
  let rep1 = find elt1 in
  let rep2 = find elt2 in
  if rep1 <> rep2 then link rep1 rep2

let contents elt =
  let info = info_of_repr (find elt) in
  info.contents

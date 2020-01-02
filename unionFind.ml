
type 'a element = {
    mutable link : 'a link;
  }
                
and 'a link =
  | Rep of 'a info
  | Parent of 'a element

and 'a info = {
    mutable rank : int;
    mutable contents : 'a;
  }
    
let fresh contents =
  { link = Rep { rank = 0; contents } }

let link rep1 rep2 =
  let info1 = match rep1.link with | Rep i -> i | Parent _ -> assert false in
  let info2 = match rep2.link with | Rep i -> i | Parent _ -> assert false in
  if info1.rank > info2.rank then
    rep2.link <- Parent rep1
  else begin
      rep1.link <- Parent rep2;
      if info1.rank = info2.rank then
        info2.rank <- info2.rank + 1
  end
  
let rec find elt =
  match elt.link with
  | Rep _ ->
     elt
  | Parent p ->
     let new_p = find p in
     elt.link <- Parent new_p;
     new_p

let union elt1 elt2 =
  let rep1 = find elt1 in
  let rep2 = find elt2 in
  if rep1 <> rep2 then link rep1 rep2


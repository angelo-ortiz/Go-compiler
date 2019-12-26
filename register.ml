
type t = string

let counter = ref 0
       
let fresh () =
  incr counter;
  Format.sprintf "#%d" !counter

module S = Set.Make(String)
type set = S.t

let rax = "%rax"
let rdx = "%rdx"
let rdi = "%rdi"
let rsp = "%rsp"
let rbp = "%rbp"
        
let parameters = [ rdi; "%rsi"; rdx; "%rcx"; "%r8"; "%r9" ] 
             
let callee_saved = [ "%rbx"; rbp; "%r12"; "%r13"; "%r14"; "%r15" ]

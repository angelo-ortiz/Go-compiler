
type t = string

let counter = ref 0
       
let fresh () =
  incr counter;
  Format.sprintf "#%d" !counter

module S = Set.Make(String)
type set = S.t

module M = Map.Make(String)
type 'a map = 'a M.t
         
let rax = "%rax"
let rdx = "%rdx"
let rdi = "%rdi"
let rsp = "%rsp"
let rbp = "%rbp"
        
let parameters = [ rdi; "%rsi"; rdx; "%rcx"; "%r8"; "%r9" ] 
             
let callee_saved = [ "%rbx"; rbp; "%r12"; "%r13"; "%r14"; "%r15" ]
let caller_saved = [ rax; "%rcx"; rdx; "%rsi"; rdi; rsp; "%r8"; "%r9"; "%r10"; "%r11" ]

let string_of_reg fmt r =
  Format.fprintf fmt "%s" r

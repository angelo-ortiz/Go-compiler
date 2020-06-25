
type t = string

module S = Set.Make(String)
type set = S.t

module M = Map.Make(String)
type 'a map = 'a M.t

let counter = ref 0
       
let fresh () =
  incr counter;
  Format.sprintf "#%d" !counter

let rax = "%rax"
let rbx = "%rbx"
let rcx = "%rcx"
let rdx = "%rdx"
let rsi = "%rsi"
let rdi = "%rdi"
let rbp = "%rbp"
let rsp = "%rsp"
let r8  = "%r8"
let r9  = "%r9"
let r10 = "%r10"
let r11 = "%r11"
let r12 = "%r12"
let r13 = "%r13"
let r14 = "%r14"
let r15 = "%r15"

let tmp1 = r11
let tmp2 = r15
  
let parameters = [ rdi; rsi; rdx; rcx; r8; r9 ] 
             
let callee_saved = [ rbx; (* rbp; *) r12; r13; r14; (* r15 *) ]
let caller_saved = rax :: r10 :: (* r11 :: *) parameters

let allocable = S.of_list (callee_saved @ caller_saved)
                 
let string_of_reg fmt r =
  Format.fprintf fmt "%s" r

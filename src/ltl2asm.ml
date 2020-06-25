
open Ltl
open X86_64

let visited = Hashtbl.create 32
let labels = Hashtbl.create 16
let instructions = ref []
let data_strings = Hashtbl.create 32
let max_imm32 = Int64.of_int32 Int32.max_int
let min_imm32 = Int64.of_int32 Int32.min_int

let inv_ubranch = function
  | Rtl.Mjz -> Rtl.Mjnz
  | Rtl.Mjnz -> Rtl.Mjz
  | Rtl.Mjei n -> Rtl.Mjnei n
  | Rtl.Mjnei n -> Rtl.Mjei n
  | Rtl.Mjgi n -> Rtl.Mjlei n
  | Rtl.Mjgei n -> Rtl.Mjli n
  | Rtl.Mjli n -> Rtl.Mjgei n
  | Rtl.Mjlei n -> Rtl.Mjgi n

let inv_bbranch = function
  | Rtl.Mje -> Rtl.Mjne
  | Rtl.Mjne -> Rtl.Mje
  | Rtl.Mjg -> Rtl.Mjle
  | Rtl.Mjge -> Rtl.Mjl
  | Rtl.Mjl -> Rtl.Mjge
  | Rtl.Mjle -> Rtl.Mjg  

let is_imm32 n =
  Int64.compare n max_imm32 < 0 && Int64.compare n min_imm32 > 0
            
let need_label l =
  Hashtbl.add labels l ()

let lookup graph l =
  Label.M.find l graph

let emit l i =
  instructions := (l, i) :: !instructions

let label_of_string s =  
  let l =
    try Hashtbl.find data_strings s
    with Not_found -> 
      let l = Label.fresh () in
      Hashtbl.add data_strings s l;
      l
  in
  Label.to_string l

let register r =
  if r = Register.rax then X86_64.rax
  else if r = Register.rbx then X86_64.rbx
  else if r = Register.rcx then X86_64.rcx
  else if r = Register.rdx then X86_64.rdx
  else if r = Register.rsi then X86_64.rsi
  else if r = Register.rdi then X86_64.rdi
  else if r = Register.rbp then X86_64.rbp
  else if r = Register.rsp then X86_64.rsp
  else if r = Register.r8  then X86_64.r8
  else if r = Register.r9  then X86_64.r9
  else if r = Register.r10 then X86_64.r10
  else if r = Register.r11 then X86_64.r11
  else if r = Register.r12 then X86_64.r12
  else if r = Register.r13 then X86_64.r13
  else if r = Register.r14 then X86_64.r14
  else if r = Register.r15 then X86_64.r15
  else assert false

let low_byte_reg r =
  if r = Register.rax then X86_64.al
  else if r = Register.rbx then X86_64.bl
  else if r = Register.rcx then X86_64.cl
  else if r = Register.rdx then X86_64.dl
  else if r = Register.rsi then X86_64.sil
  else if r = Register.rdi then X86_64.dil
  else if r = Register.rbp then X86_64.bpl
  else if r = Register.rsp then X86_64.spl
  else if r = Register.r8  then X86_64.r8b
  else if r = Register.r9  then X86_64.r9b
  else if r = Register.r10 then X86_64.r10b
  else if r = Register.r11 then X86_64.r11b
  else if r = Register.r12 then X86_64.r12b
  else if r = Register.r13 then X86_64.r13b
  else if r = Register.r14 then X86_64.r14b
  else if r = Register.r15 then X86_64.r15b
  else assert false

let x_reg r =
  X86_64.(!%) (register r)

let ind ?(reg=Register.rbp) ofs =
  X86_64.ind ~ofs (register reg)
  
let operand = function
  | Colouring.Reg mr -> x_reg mr
  | Colouring.Spilt n -> ind n
  | Colouring.Heap _ -> assert false
    
let q_tmp1 = x_reg Register.tmp1
let b_tmp1 = X86_64.(!%) (low_byte_reg Register.tmp1) 
let q_tmp2 = x_reg Register.tmp2

let ubranch br c2 j_l =
  let n1, x_br = 
    match br with
    | Rtl.Mjz -> 0L, X86_64.jz
    | Rtl.Mjnz -> 0L, X86_64.jnz
    | Rtl.Mjei n -> n, X86_64.je
    | Rtl.Mjnei n -> n, X86_64.jne
    | Rtl.Mjgi n -> n, X86_64.jg
    | Rtl.Mjgei n -> n, X86_64.jge
    | Rtl.Mjli n -> n, X86_64.jl
    | Rtl.Mjlei n -> n, X86_64.jle
  in
  let c2_op = operand c2 in
  if n1 = 0L then X86_64.movq c2_op q_tmp1 ++ X86_64.testq q_tmp1 q_tmp1
  else if is_imm32 n1 then X86_64.cmpq (X86_64.imm32 (Int64.to_int32 n1)) c2_op
  else X86_64.movq (X86_64.imm64 n1) q_tmp1 ++ X86_64.cmpq q_tmp1 c2_op;
  ++ x_br (Label.to_string j_l)

let bbranch br c1 c2 j_l =
  let x_br = 
    match br with
    | Rtl.Mje -> X86_64.je
    | Rtl.Mjne -> X86_64.jne
    | Rtl.Mjg -> X86_64.jg
    | Rtl.Mjge -> X86_64.jge
    | Rtl.Mjl -> X86_64.jl
    | Rtl.Mjle -> X86_64.jle
  in
  X86_64.cmpq (operand c1) (operand c2)
  ++ x_br (Label.to_string j_l)

let uset op c =
  let x_set, n = 
    match op with
    | Ertl.Msetei n -> X86_64.sete, n
    | Ertl.Msetnei n -> X86_64.setne, n
    | Ertl.Msetgi n -> X86_64.setg, n
    | Ertl.Msetgei n -> X86_64.setge, n
    | Ertl.Msetli n -> X86_64.setl, n
    | Ertl.Msetlei n -> X86_64.setle, n
    | _ -> assert false
  in
  begin
    if is_imm32 n then X86_64.cmpq (X86_64.imm32 (Int64.to_int32 n)) c
    else X86_64.movq (X86_64.imm64 n) q_tmp1 ++ X86_64.cmpq q_tmp1 c
  end
  ++ x_set          b_tmp1
  ++ X86_64.movzbq  b_tmp1 (register Register.tmp1)
  ++ X86_64.movq    q_tmp1 c
  
let bset op c1 c2 =
  let x_set = 
    match op with
    | Ertl.Msete -> X86_64.sete
    | Ertl.Msetne -> X86_64.setne
    | Ertl.Msetg -> X86_64.setg
    | Ertl.Msetge -> X86_64.setge
    | Ertl.Msetl -> X86_64.setl
    | Ertl.Msetle -> X86_64.setle
    | _ -> assert false
  in
  X86_64.cmpq    c1 c2 ++
  x_set          b_tmp1 ++
  X86_64.movzbq  b_tmp1 (register Register.tmp1) ++
  X86_64.movq    q_tmp1 c2
  
let rec lin graph l =
  if not (Hashtbl.mem visited l) then begin
      Hashtbl.add visited l ();
      instr graph l (lookup graph l)
    end else begin
      need_label l;
      emit (Label.fresh ()) (X86_64.jmp (Label.to_string l))
    end
  
and instr graph l = function
  | Iint (n, r, next_l) ->
     let op_r = operand r in
     begin
       match r with
       | Reg mr when n = 0L -> emit l (X86_64.xorq op_r op_r)
       | _ -> emit l (X86_64.movq (X86_64.imm64 n) (operand r))
     end;
     lin graph next_l
  | Istring (s, r, next_l) ->
     let s_lab = label_of_string s in
     emit l (X86_64.leaq (X86_64.lab s_lab) (register r));
     lin graph next_l
  | Ibool (b, c, next_l) ->
     let inst =
       match c with 
       | Reg mr when not b ->
          let c' = x_reg mr in
          X86_64.xorq c' c'
       | _ ->
          let n = if b then 1l else 0l in
          X86_64.movq (X86_64.imm32 n) (operand c)
     in
     emit l inst;
     lin graph next_l
  | Ilea (src, ofs, dst, next_l) ->
     emit l (X86_64.leaq (ind ~reg:src ofs) (register dst));
     lin graph next_l
  | Iload (src, ofs, dst, next_l) ->
     emit l (X86_64.movq (ind ~reg:src ofs) (x_reg dst));
     lin graph next_l
  | Istore (src, dst, ofs, next_l) ->
     emit l (X86_64.movq (x_reg src) (ind ~reg:dst ofs));
     lin graph next_l
  | Icall (f, next_l) ->
     emit l (X86_64.call f);
     lin graph next_l
  | Imunop (op, c, next_l) ->
     let c_op = operand c in
     begin
       match op with
       | Ertl.Mnot ->
          emit l (X86_64.notq c_op)
       | Ertl.Mneg ->
          emit l (X86_64.negq c_op)
       | Ertl.Maddi n ->
          let code =
            if is_imm32 n then X86_64.addq (X86_64.imm32 (Int64.to_int32 n)) c_op
            else X86_64.movq (X86_64.imm64 n) q_tmp1 ++ X86_64.addq q_tmp1 c_op
          in
          emit l code
       | Ertl.Mimuli n ->
          let code =
            if is_imm32 n then X86_64.imulq (X86_64.imm32 (Int64.to_int32 n)) c_op
            else X86_64.movq (X86_64.imm64 n) q_tmp1 ++ X86_64.imulq q_tmp1 c_op
          in
          emit l code
       | Ertl.Minc ->
          emit l (X86_64.incq c_op)
       | Ertl.Mdec ->
          emit l (X86_64.decq c_op)
       | Ertl.Msetei _ | Ertl.Msetnei _ | Ertl.Msetgi _
       | Ertl.Msetgei _ | Ertl.Msetli _ | Ertl.Msetlei _ as op ->
          emit l (uset op c_op)
     end;
     lin graph next_l
  | Iidiv_imm (n, next_l) -> (* idiv does not take immediate values *)
     emit l (X86_64.movq (X86_64.imm64 n) q_tmp1 ++ X86_64.cqto ++ X86_64.idivq q_tmp1);
     lin graph next_l
  | Iidiv (c, next_l) ->
     emit l (X86_64.cqto ++ X86_64.idivq (operand c));
     lin graph next_l
  | Iinc_dec (op, r, ofs, next_l) ->
     let inc_dec = match op with Rtl.IDinc -> X86_64.incq | Rtl.IDdec -> X86_64.decq
     in
     emit l (inc_dec (ind ~reg:r ofs));
     lin graph next_l
  | Imbinop (op, c1, c2, next_l) ->
     let c1_op = operand c1 in
     let c2_op = operand c2 in
     emit l (
         match op with
         | Ertl.Madd -> X86_64.addq c1_op c2_op
         | Ertl.Msub -> X86_64.subq c1_op c2_op
         | Ertl.Mimul -> X86_64.imulq c1_op c2_op
         | Ertl.Mxor -> X86_64.xorq c1_op c2_op
         | Ertl.Mmov -> X86_64.movq c1_op c2_op
         | Ertl.Msete | Ertl.Msetne | Ertl.Msetg | Ertl.Msetge
         | Ertl.Msetl | Ertl.Msetle as op -> bset op c1_op c2_op
       );
     lin graph next_l
  | Imubranch (br, c, true_l, false_l) when not (Hashtbl.mem visited false_l) ->
     need_label true_l;
     emit l (ubranch br c true_l);
     lin graph false_l;
     if not (Hashtbl.mem visited true_l) then lin graph true_l
  | Imubranch (br, c, true_l, false_l) when not (Hashtbl.mem visited true_l) ->
     instr graph l (Imubranch (inv_ubranch br, c, false_l, true_l))
  | Imubranch (br, c, true_l, false_l) ->
     need_label true_l;
     need_label false_l;
     emit l (ubranch br c true_l);
     emit l (X86_64.jmp (Label.to_string false_l))
  | Imbbranch (br, c1, c2, true_l, false_l) when not (Hashtbl.mem visited false_l) ->
     need_label true_l;
     emit l (bbranch br c1 c2 true_l);
     lin graph false_l;
     if not (Hashtbl.mem visited true_l) then lin graph true_l
  | Imbbranch (br, c1, c2, true_l, false_l) when not (Hashtbl.mem visited true_l) ->
     instr graph l (Imbbranch (inv_bbranch br, c1, c2, false_l, true_l))
  | Imbbranch (br, c1, c2, true_l, false_l) ->
     need_label true_l;
     need_label false_l;
     emit l (bbranch br c1 c2 true_l);
     emit l (X86_64.jmp (Label.to_string false_l))
  | Igoto next_l ->
     if Hashtbl.mem visited next_l then begin
         need_label next_l;
         emit l (X86_64.jmp (Label.to_string next_l))
       end else begin
         emit l X86_64.nop;
         lin graph next_l
       end
  | Ipush (c, next_l) ->
     emit l (X86_64.pushq (operand c));
     lin graph next_l
  | Ipop (r, next_l) ->
     emit l (X86_64.popq (register r));
     lin graph next_l
  | Ireturn  ->
     emit l ret

let programme p =
  let text =
    X86_64.globl "main" ++
      Utils.Smap.fold (fun f { entry; body } txt ->
          let entry, body = Branch.compress entry body in
          lin body entry;
          let instrs = !instructions in
          instructions := [];
          X86_64.label f
          ++ List.fold_left (fun txt (l, code) ->
                 let txt = code ++ txt in
                 if Hashtbl.mem labels l then X86_64.label (Label.to_string l) ++ txt
                 else txt
               ) txt instrs
        ) p (X86_64.inline "\n")
  in
  { text; data = Hashtbl.fold (fun str l data ->
                     data ++ X86_64.label (Label.to_string l) ++ X86_64.string str
                   ) data_strings (X86_64.inline "")
  }

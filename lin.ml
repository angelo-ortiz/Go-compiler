
open Register
open Label
open Colouring
open Ltltree
open Branch
open X86_64

let visited = Hashtbl.create 32
let labels = Hashtbl.create 16
let instructions = ref []
let data_strings = ref []
                 
let need_label l =
  Hashtbl.add labels l ()

let lookup graph l =
  Label.M.find l graph

let emit l i =
  instructions := (l, i) :: !instructions

let label_of_string s =
  let l = Label.fresh () in
  data_strings := (l, s) :: !data_strings;
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
  !% (register r)

let ind ?(reg=Register.rbp) ofs =
  X86_64.ind ~ofs (register reg)
  
let operand = function
  | Reg mr ->
     x_reg mr
  | Spilled n ->
     ind n

let ubranch br c2 j_l =
  let n1, x_br = 
    match br with
    | Rtltree.Mjz ->
       0l, jz
    | Rtltree.Mjnz ->
       0l, jnz
    | Rtltree.Mjei n ->
       n, je
    | Rtltree.Mjnei n ->
       n, jne
    | Rtltree.Mjgi n ->
       n, jg
    | Rtltree.Mjgei n ->
       n, jge
    | Rtltree.Mjli n ->
       n, jl
    | Rtltree.Mjlei n ->
       n, jle
  in
  cmpq (imm32 n1) (operand c2) ++
  x_br (Label.to_string j_l)

let bbranch br c1 c2 j_l =
  let x_br = 
    match br with
    | Rtltree.Mje ->
       je
    | Rtltree.Mjne ->
       jne
    | Rtltree.Mjg ->
       jg
    | Rtltree.Mjge ->
       jge
    | Rtltree.Mjl ->
       jl
    | Rtltree.Mjle ->
       jle
  in
  cmpq (operand c1) (operand c2) ++
    x_br (Label.to_string j_l)

let x_tmp1 = x_reg Register.tmp1
let x_tmp2 = x_reg Register.tmp2

let uset op c =
  let cond_mov, n = 
    match op with
    | Ertltree.Msetei n ->
       cmove, n
    | Ertltree.Msetnei n ->
       cmovne, n
    | Ertltree.Msetgi n ->
       cmovg, n
    | Ertltree.Msetgei n ->
       cmovge, n
    | Ertltree.Msetli n ->
       cmovl, n
    | Ertltree.Msetlei n ->
       cmovle, n
    | _ ->
       assert false
  in
  movq      c x_tmp1 ++
  movq      (imm32 0l) c ++
  movq      (imm32 1l) x_tmp2 ++
  cmpq      (imm32 n) x_tmp1 ++
  cond_mov  x_tmp2 c
  
let bset op c1 c2 =
  let cond_mov = 
    match op with
    | Ertltree.Msete ->
       cmove
    | Ertltree.Msetne ->
       cmovne
    | Ertltree.Msetg ->
       cmovg
    | Ertltree.Msetge ->
       cmovge
    | Ertltree.Msetl ->
       cmovl
    | Ertltree.Msetle ->
       cmovle
    | _ ->
       assert false
  in
  movq     c2 x_tmp1 ++
  movq     (imm32 0l) c2 ++
  movq     (imm32 1l) x_tmp2 ++
  cmpq     c1 x_tmp1 ++
  cond_mov x_tmp2 c2
  
let rec lin graph l =
  if not (Hashtbl.mem visited l) then begin
      Hashtbl.add visited l ();
      instr graph l (lookup graph l)
    end else begin
      need_label l;
      emit (Label.fresh ()) (jmp (Label.to_string l))
    end
  
and instr graph l = function
  | Iint (n, r, next_l) ->
     emit l (X86_64.movq (X86_64.imm32 n) (operand r));
     lin graph next_l
  | Istring (s, r, next_l) ->
     let s_lab = label_of_string s in
     emit l (leaq (lab s_lab) (register r));
     lin graph next_l
  | Ibool (b, c, next_l) ->
     emit l (movq (imm32 1l) (operand c));
     lin graph next_l
  | Ilea (src, ofs, dst, next_l) ->
     emit l (leaq (ind ~reg:src ofs) (register dst));
     lin graph next_l
  | Iload (src, ofs, dst, next_l) ->
     emit l (movq (ind ~reg:src ofs) (x_reg dst));
     lin graph next_l
  | Istore (src, dst, ofs, next_l) ->
     emit l (movq (x_reg src) (ind ~reg:dst ofs));
     lin graph next_l
  | Icall (f, next_l) ->
     emit l (call f);
     lin graph next_l
  | Imunop (op, c, next_l) ->
     let c_op = operand c in
     begin
       match op with
       | Ertltree.Mnot ->
          emit l (notq c_op)
       | Ertltree.Mneg ->
          emit l (negq c_op)
       | Ertltree.Maddi n ->
          emit l (addq (imm32 n) c_op)
       | Ertltree.Mimuli n ->
          emit l (imulq (imm32 n) c_op)
       | Ertltree.Minc ->
          emit l (incq c_op)
       | Ertltree.Mdec ->
          emit l (decq c_op)
       | Ertltree.Msetei _ | Ertltree.Msetnei _ | Ertltree.Msetgi _ | Ertltree.Msetgei _
       | Ertltree.Msetli _ | Ertltree.Msetlei _ as op ->
          emit l (uset op c_op)
     end;
     lin graph next_l
  | Iidiv_imm (n, next_l) ->
     emit l (movq (imm32 n) x_tmp1 ++ cqto ++ idivq x_tmp1);
     lin graph next_l
  | Iidiv (c, next_l) ->
     emit l (cqto ++ idivq (operand c));
     lin graph next_l
  | Iinc_dec (op, r, ofs, next_l) ->
     let inc_dec = match op with Rtltree.IDinc -> incq | Rtltree.IDdec -> decq in
     emit l (inc_dec (ind ~reg:r ofs));
     lin graph next_l
  | Imbinop (op, c1, c2, next_l) ->
     let c1_op = operand c1 in
     let c2_op = operand c2 in
     begin
       match op with
       | Ertltree.Madd ->
          emit l (addq c1_op c2_op)
       | Ertltree.Msub ->
          emit l (subq c1_op c2_op)
       | Ertltree.Mimul ->
          emit l (imulq c1_op c2_op)
       | Ertltree.Mxor ->
          emit l (xorq c1_op c2_op)
       | Ertltree.Mmov ->
          emit l (movq c1_op c2_op)
       | Ertltree.Msete | Ertltree.Msetne | Ertltree.Msetg | Ertltree.Msetge
       | Ertltree.Msetl | Ertltree.Msetle as op ->
          emit l (bset op c1_op c2_op)
     end;
     lin graph next_l
  | Imubranch (br, c, true_l, false_l) when not (Hashtbl.mem visited false_l) ->
     need_label true_l;
     emit l (ubranch br c true_l);
     lin graph false_l;
     if not (Hashtbl.mem visited true_l) then lin graph true_l
  | Imubranch (br, c, true_l, false_l) when not (Hashtbl.mem visited true_l) ->
     instr graph l (Imubranch (Utils.inv_ubranch br, c, false_l, true_l))
  | Imubranch (br, c, true_l, false_l) ->
     need_label true_l;
     need_label false_l;
     emit l (ubranch br c true_l);
     emit l (jmp (Label.to_string false_l))
  | Imbbranch (br, c1, c2, true_l, false_l) when not (Hashtbl.mem visited false_l) ->
     need_label true_l;
     emit l (bbranch br c1 c2 true_l);
     lin graph false_l;
     if not (Hashtbl.mem visited true_l) then lin graph true_l
  | Imbbranch (br, c1, c2, true_l, false_l) when not (Hashtbl.mem visited true_l) ->
     instr graph l (Imbbranch (Utils.inv_bbranch br, c1, c2, false_l, true_l))
  | Imbbranch (br, c1, c2, true_l, false_l) ->
     need_label true_l;
     need_label false_l;
     emit l (bbranch br c1 c2 true_l);
     emit l (jmp (Label.to_string false_l))
  | Igoto next_l ->
     if Hashtbl.mem visited next_l then begin
         need_label next_l;
         emit l (jmp (Label.to_string next_l))
       end else begin
         emit l nop;
         lin graph next_l
       end
  | Ipush (c, next_l) ->
     emit l (pushq (operand c));
     lin graph next_l
  | Ipop (r, next_l) ->
     emit l (popq (register r));
     lin graph next_l
  | Ireturn  ->
     emit l ret

let programme p =
  let text =
    globl "main" ++
      Asg.Smap.fold (fun f { entry; body } txt ->
          let entry, body = Branch.compress entry body in
          lin body entry;
          let instrs = !instructions in
          instructions := [];
          label f ++
            List.fold_left (fun txt (l, code) ->
                let txt = code ++ txt in
                if Hashtbl.mem labels l then label (Label.to_string l) ++ txt else txt
              ) txt instrs
        ) p (inline "\n")
  in
  { text; data = List.fold_left (fun data (l, str) ->
                     data ++ label (Label.to_string l) ++ string str
                   ) (inline "") !data_strings
  }

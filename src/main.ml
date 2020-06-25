
(* Go compiler's entry point *)

open Lexing
open Parser

let usage = "usage: pgoc [options] file.go"

let parse_only = ref false
let type_only = ref false

let spec =
  [ "--parse-only", Arg.Set parse_only, " stop after parsing";
    "--type-only", Arg.Set type_only, " stop after typing"; ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".go") then raise (Arg.Bad "no .go extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with
  | Some f -> f
  | None -> Arg.usage spec usage; exit 1

let report loc =
  let line, fst_char, last_char = Utils.position_of_loc loc in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:\n" file line fst_char last_char

let () =
  let ch = open_in file in
  let lb = Lexing.from_channel ch in
  try
    let p = Parser.file Lexer.next_token lb in
    close_in ch;
    if !parse_only then exit 0;
    let p = TypeChecker.programme p in
    if !type_only then exit 0;
    let p = Asg2isl.programme p in
    (* Pp.is_file p; *)
    let p = Isl2rtl.programme p in
    let p = Rtl2ertl.programme p in
    (* Pp.ertl_file p; *)
    let p = Ertl2ltl.programme p in
    (* Pp.ltl_file p; *)
    let code = Ltl2asm.programme p in
    let ch = open_out (Filename.chop_suffix file ".go" ^ ".s") in
    let fmt = Format.formatter_of_out_channel ch in
    X86_64.print_program fmt code;
    close_out ch
  with
  | Lexer.Lexing_error s ->
     report (lexeme_start_p lb, lexeme_end_p lb);
     Format.eprintf "%slexical error%s: %s@." Utils.red Utils.close s;
     exit 1
  | Parser.Error ->
     report (lexeme_start_p lb, lexeme_end_p lb);
     Format.eprintf "%ssyntax error%s: %s@." Utils.red
       Utils.close (Utils.format_mid_string "unexpected token " (lexeme lb) "");
     exit 1
  | AstUtils.Syntax_error (loc, msg) ->
     report loc;
     Format.eprintf "%ssyntax error%s: %s@." Utils.red Utils.close msg;
     exit 1
  | TypeChecker.Type_error (loc, msg) ->
     report loc;
     Format.eprintf "%stype error%s: %s@." Utils.red Utils.close msg;
     exit 1
  | e ->
     Format.eprintf "%sunrecognised error%s: %s%s%s@."
       Utils.red Utils.close Utils.blue (Printexc.to_string e) Utils.close;
     exit 2

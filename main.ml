
(* Go compiler's entry point *)

open Lexing
open Parser
open Utils

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

let () = Printexc.record_backtrace true
  
let () =
  let ch = open_in file in
  let lb = Lexing.from_channel ch in
  try
    let ast_file = Parser.file Lexer.next_token lb in
    close_in ch;
    if !parse_only then exit 0;
    let type_file = Type_checker.type_file ast_file in
    if !type_only then exit 0;
    let programme = Is.file type_file in
    let programme = Rtl.file programme in
    Format.printf "**  ====== RTL =====  **\n";
    Pp.rtl_file programme;
    Format.printf "**  === RTL done ===  **\n\n";
    let programme = Ertl.file programme in
    Pp.ertl_file programme;
    let programme = Ltl.file programme in
    Pp.ltl_file programme
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
  | Utils.Syntax_error (loc, msg) ->
     report loc;
     Format.eprintf "%ssyntax error%s: %s@." Utils.red Utils.close msg;
     exit 1
  | Utils.Type_error (loc, msg) ->
     report loc;
     Format.eprintf "%stype error%s: %s@." Utils.red Utils.close msg;
     exit 1
  | e ->
     let msg = Printexc.to_string e
     and stack = Printexc.get_backtrace () in
     Printf.eprintf "there was an error: %s%s\n" msg stack;
     Format.eprintf "%sunrecognised error%s: %s%s%s@."
       Utils.red Utils.close Utils.blue (Printexc.to_string e) Utils.close;
     exit 2

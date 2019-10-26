
(* Go compiler's entry point *)

open Lexing
open Parser

let usage = "usage: pgoc [options] file.go"

let parse_only = ref true (* false *)
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

let report (b, e) =
  let line = b.pos_lnum in
  let fchar = b.pos_cnum - b.pos_bol + 1 in
  let lchar = e.pos_cnum - e.pos_bol +1 in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:\n" file line fchar lchar

let () =
  let ch = open_in file in
  let lb = Lexing.from_channel ch in
  try
    let f = Parser.file Lexer.next_token lb in
    close_in ch;
    if !parse_only then exit 0;
    (* typing here *)
    if !type_only then exit 0;
  with
  | Lexer.Lexing_error s ->
     report (lexeme_start_p lb, lexeme_end_p lb);
     Format.eprintf "lexical error: \027[91m%s\027[0m@." s;
     exit 1
  | Parser.Error ->
     report (lexeme_start_p lb, lexeme_end_p lb);
     Format.eprintf "syntax error: \027[91m%s\027[0m@." (lexeme lb);
     exit 1
  (* typing here *)
  | e ->
     Format.eprintf "unrecognised error: \027[91m%s\027[0m@." (Printexc.to_string e);
     exit 2

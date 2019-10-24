{
  open Lexing
  open Parser

  exception Lexing_error of string

  let id_or_kwd =
    let h = Hashtbl.create 17 in
	List.iter (fun (w, t) -> Hashtbl.add h w t)
	  [ "import", IMPORT; "package", PACKAGE; "return", RETURN;
	  	"func", FUNC; "var", VAR; "struct", STRUCT; "for", FOR;
		"type", TYPE; "if", IF; "else", ELSE; "nil", CST Cnil;
	  	"true", CST (Cbool true); "false", CST (Cbool false); ];
	fun id -> try Hashtbl.find h id
		   	  with Not_found -> IDENT id

}

let decimal = ['0' '9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | decimal)*
let hexa = decimal | ['a'-'f' 'A'-'F']
let integer = decimal+ | '0' ['x' 'X'] hexa+
let space = [' ' '\t']
let char = '\'' ([^'\\' '"' '\''] | '\\' ['\\' '"' 'n' 't']) '\''
TODO add ; automatically
rule token = parse
  | "//" [^'\n']* '\n'
  | '\n'					{ new_line lexbuf; token lexbuf }
  | space+					{ token lexbuf }
  | "package" 				{ package lexbuf }
  | "fmt" space* "." 		{ print lexbuf }
  | integer as n			{ CST (Cint (Int64.of_string n)) }
  | '"' char* '"' as str	{ CST (Cstring str) }
  | "/*"	  	  	 		{ comment lexbuf }
  | ident as str			{ id_or_kwd str }
  | _ as c	  	  	 		{ raise (Lexing_error (Format.sprintf "unexpected %c@." c)) }
  | "//" [^'\n']* eof
  | eof 	  	  	 		{ EOF }

and package = parse
  | '\n'					{ new_line lexbuf; package lexbuf }
  | space+					{ package lexbuf }
  | "main"					{ PACKAGE IF }
  | _						{ raise (Lexing_error "unknown package") }
  | eof						{ raise (Lexing_error "no package given") }

and print = parse
  | '\n'					{ new_line lexbuf; print lexbuf }
  | space+					{ print lexbuf }
  | "Print"					{ PRINT }
  | _ 						{ raise (Lexing_error "unknown fmt function") }
  | eof						{ raise (Lexing_error "no function given") }

and	comment = parse
  | '\n'					{ new_line lexbuf; comment lexbuf }
  | [^'\n']* "*/"			{ token lexbuf }
  | eof						{ raise (Lexing_error "comment not terminated") }
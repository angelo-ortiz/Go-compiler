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
		   	  with Not_found -> Ident id

}

let decimal = ['0' '9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let hexa = decimal | ['a'-'f' 'A'-'F']
let integer = decimal+ | '0' ['x' 'X'] hexa+
let char = '\'' ([^'\' '"' '\''] | '\' ['\' '"' 'n' 't']) '\''

rule token = parse
  | '\n'					{ new_line lexbuf; TODO: automatic semicolon}
  | integer as n			{ CST (int_of_string n) }
  | '"' char* '"' as str	{ CST str }

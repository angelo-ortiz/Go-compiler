{

  (* Go lexer *)
 
  open Lexing
  open Parser
  open Utils

  exception Lexing_error of string

  let id_or_kwd =
    let h = Hashtbl.create 17 in
    List.iter (fun (w, t) -> Hashtbl.add h w t) [
    	"return", RETURN; "func", FUNC; "var", VAR;
	"struct", STRUCT; "type", TYPE; "for", FOR;
	"if", IF; "else", ELSE; "nil", CST Cnil;
	"true", CST (Cbool true); "false", CST (Cbool false)
      ];
    fun id -> try Hashtbl.find h id with Not_found -> IDENT id

  let str_buf = Buffer.create 1024
  let str_start_p = ref None

  let set_start_pos lb =
      match !str_start_p with
      | None -> assert false
      | Some p -> lb.lex_start_p <- p

  let smcolon_state = ref None

  let update_smcolon () =
    if !smcolon_state = Some false then smcolon_state := Some true
}

let decimal = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | decimal)*
let hexa = decimal | ['a'-'f' 'A'-'F']
let integer = decimal+ | '0' ['x' 'X'] hexa+
let space = [' ' '\t']
let chr = '\'' ([^'\\' '\"' '\''] | '\\' ['\\' '\"' 'n' 't']) '\''

rule token = parse
  | "//" [^'\n']* '\n'
  | '\n'		{ new_line lexbuf; update_smcolon (); token lexbuf }
  | space+		{ token lexbuf }
  | "import" 		{ import lexbuf }
  | "package" 		{ package lexbuf }
  | integer as n	{ INT n }
  | '"'			{ str_start_p := Some lexbuf.lex_start_p; CST (Cstring (string lexbuf)) }
  | "/*"	  	{ comment lexbuf }
  | ":="		{ ASSIGN }
  | "=="		{ CMP Beq }
  | "!="		{ CMP Bneq }
  | "<"			{ CMP Blt }
  | "<="		{ CMP Ble }
  | ">"			{ CMP Bgt }
  | ">="		{ CMP Bge }
  | "&&"		{ AND }
  | "||"		{ OR }
  | "++"		{ INCR }
  | "--"		{ DECR }
  | '='			{ SET }
  | '{'			{ BEGIN }
  | '}'			{ END }
  | '('			{ LPAR }
  | ')'			{ RPAR }
  | '+'			{ PLUS }
  | '-'			{ MINUS }
  | '*'			{ STAR }
  | '/'			{ DIV }
  | '%'			{ MOD }
  | '&'			{ ADDR }
  | '!'			{ NOT }
  | ','			{ COMMA }
  | ';'			{ SMCOLON }
  | '.'			{ DOT }
  | ident as str	{ id_or_kwd str }
  | "//" [^'\n']* eof
  | eof 	    	{ EOF }
  | _ as c	    	{ raise (Lexing_error (Utils.format_mid_string
						"unexpected character " (String.make 1 c) "")) }

and string = parse
  | '"'   		{ let str = Buffer.contents str_buf in
  			  Buffer.reset str_buf; set_start_pos lexbuf; str }
  | '%'   		{ Buffer.add_string str_buf "%%"; string lexbuf }
  | "\\\\"		{ Buffer.add_char str_buf '\\'; string lexbuf }
  | "\\\""		{ Buffer.add_char str_buf '\"'; string lexbuf }
  | "\\n"		{ Buffer.add_char str_buf '\n'; string lexbuf }
  | "\\t"		{ Buffer.add_char str_buf '\t'; string lexbuf }
  | _ as c		{ Buffer.add_char str_buf c; string lexbuf }
  | eof	 		{ raise (Lexing_error (Format.sprintf "string not terminated")) }

and import = parse
  | '\n'		{ new_line lexbuf; import lexbuf }
  | space+		{ import lexbuf }
  | "\"fmt\""		{ IMPORT }
  | _			{ raise (Lexing_error "unknown package") }
  | eof			{ raise (Lexing_error (Utils.format_mid_string
						"unexpected " "eof" ", expecting a package name")) }

and package = parse
  | '\n'		{ new_line lexbuf; package lexbuf }
  | space+		{ package lexbuf }
  | "main"		{ PACKAGE }
  | _			{ raise (Lexing_error (Utils.format_mid_string
						"package name must be " "main" "")) }
  | eof			{ raise (Lexing_error (Utils.format_mid_string
						"unexpected " "eof" ", expecting a package name")) }

and comment = parse
  | "*/"		{ token lexbuf }
  | '\n'		{ new_line lexbuf; update_smcolon (); comment lexbuf }
  | _			{ comment lexbuf }
  | eof			{ raise (Lexing_error (Format.sprintf "comment not terminated")) }

{

  let next_token =
    let add_semicolon = function
      | IDENT _ | CST _ | INT _ | RETURN | INCR | DECR | RPAR | END | PACKAGE ->
         smcolon_state := Some false
      | _ ->
         smcolon_state := None
    in
    let next = ref None in
    fun lb ->
    match !next with
    | None ->
       let t = token lb in
       if !smcolon_state = Some true then begin
       	  next := Some t;
       	  SMCOLON
	end else begin
	  add_semicolon t;
	  t
	end
    | Some t ->
       add_semicolon t;
       next := None;
       t

}

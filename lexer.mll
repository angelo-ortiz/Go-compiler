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
let char = '\'' ([^'\\' '"' '\''] | '\\' ['\\' '"' 'n' 't']) '\''

rule token = parse
  | "//" [^'\n']* '\n'
  | '\n'					{ new_line lexbuf; update_smcolon (); token lexbuf }
  | space+					{ token lexbuf }
  | "import" 				{ import lexbuf }
  | "package" 				{ package lexbuf }
  | "fmt" space* "." 		{ print lexbuf }
  | integer as n			{ try CST (Cint (Int64.of_string n)) with Failure _ ->
  			   				  raise (Lexing_error (Format.sprintf "%s does not fit in 64b@." n)) }
  | '"' char* '"' as str	{ CST (Cstring str) }
  | "/*"	  	  	 		{ comment lexbuf }
  | ":="					{ ASSIGN }
  | "=="					{ CMP Beq }
  | "!="					{ CMP Bneq }
  | "<"						{ CMP Blt }
  | "<="					{ CMP Ble }
  | ">"						{ CMP Bgt }
  | ">="					{ CMP Bge }
  | "&&"					{ AND }
  | "||"					{ OR }
  |	'='						{ SET }
  | '{'						{ BEGIN }
  | '}'						{ END }
  | '('						{ LPAR }
  | ')'						{ RPAR }
  | '+'						{ PLUS }
  | '-'						{ MINUS }
  | '*'						{ STAR }
  | '/'						{ DIV }
  | '%'						{ MOD }
  | '&'						{ ADDR }
  | ','						{ COMMA }
  | ';'						{ SMCOLON }
  | ident as str			{ id_or_kwd str }
  | "//" [^'\n']* eof
  | eof 	  	  	 		{ EOF }
  | _ as c	  	  	 		{ raise (Lexing_error (Format.sprintf "unexpected %c@." c)) }

and import = parse
  | '\n'					{ new_line lexbuf; import lexbuf }
  | space+					{ import lexbuf }
  | "\"fmt\""				{ IMPORT }
  | _						{ raise (Lexing_error "unknown package") }
  | eof						{ raise (Lexing_error "no package given") }

and package = parse
  | '\n'					{ new_line lexbuf; package lexbuf }
  | space+					{ package lexbuf }
  | "main"					{ PACKAGE }
  | _						{ raise (Lexing_error "package name must be `main`") }
  | eof						{ raise (Lexing_error "no package given") }

and print = parse
  | '\n'					{ new_line lexbuf; print lexbuf }
  | space+					{ print lexbuf }
  | "Print"					{ PRINT }
  | _ 						{ raise (Lexing_error "unknown fmt function") }
  | eof						{ raise (Lexing_error "no function given") }

and	comment = parse
  | "*/"					{ token lexbuf }
  | '\n'					{ new_line lexbuf; update_smcolon (); comment lexbuf }
  | _						{ comment lexbuf }
  | eof						{ raise (Lexing_error "comment not terminated") }

{

  let next_token =
    let add_semicolon = function
	  | IDENT _ | CST _ | RETURN | INCR | DECR | RPAR | END | PACKAGE ->
	  	 smcolon_state := Some false
	  | _ -> smcolon_state := None
	in
	let next = ref None in
	fun lb ->
	  match !next with
	  | None ->
	  	 let t = token lb in
		 if !smcolon_state = Some true then
		   begin
			 if t = END then ( smcolon_state := Some false; END )
			 else ( next := Some t; smcolon_state := None; SMCOLON )
		   end
		 else 
		   begin
			 add_semicolon t;
			 if t = SMCOLON then
			   let nt = token lb in
			   begin
			     if nt = END then ( smcolon_state := Some false; END )
				 else ( next := Some nt; smcolon_state := None; SMCOLON )
		   	   end
			 else t
		   end
	  | Some t ->
	  	 add_semicolon t;
		 next := None;
		 if t = SMCOLON then
		   let nt = token lb in
		   begin
		     if nt = END then ( smcolon_state := Some false; END )
			 else ( next := Some nt; smcolon_state := None; SMCOLON )
		   end
		 else t

}
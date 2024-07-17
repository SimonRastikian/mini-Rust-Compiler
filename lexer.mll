{
	open Lexing
	open Parsing
	open Buffer
	open Parser
	open Ast

	exception Lexing_error of string

	(* key words reserved by Rust language *)
	let kwd =
	[ "fn", FN;
	  "if", IF;
	  "else", ELSE;
	  "while", WHILE;
	  "let", LET;
	  "mut", MUT;
	  "struct", STRUCT;
	  "return", RETURN;
	  "false",CST (Cbool false);
	  "true", CST (Cbool true); ]

	(* create a buffer of size 1024 characters *)
	let string_buffer = Buffer.create 1024

	(* iterate over the key words and put the string token in a hash table *)
	let id_or_kwd =
		let h = Hashtbl.create 17 in
		List.iter (fun (s,t) -> Hashtbl.add h s t) kwd;
		fun s -> try Hashtbl.find h s with Not_found -> IDENT s
}

let digit = ['0'-'9']							(* chiffre *)
let integer = digit+							(* entier *)
let letter = ['a'-'z' 'A'-'Z']					(* lettre *)
let ident = letter (letter|digit|'_')*			(* variable *)
let cmnt = "//" [^ '\n']*						(* commentaire *)
let space = [' ' '\t' ] 						(* blanc *)

rule token = parse
	| "vec"		{VEC}
	| "len" 	{LEN}					(* .len() *)
	| "print"	{PRINT} 		(* print!("str") *)
	| integer as i 						(* creer entier *)
		{ try CST (Cint(int_of_string i))
		  with _ -> raise (Lexing_error("constant too large"^i)) }
	| ident as id { id_or_kwd id}				(* creer variable *)
	| "&&"		{AND}
	| "||"		{OR}
	| "=="		{CMP Beq}
	| "!="		{CMP Bneq}
	| "<="		{CMP Ble}
	| ">="		{CMP Bge}
	| '<'		{BLT} (* nom different que CMP pour parser dans typ *)
	| '>'		{BGT} (* nom different que CMP pour parser dans typ *)
	| '&'		{BORR}
	| '!'		{NOT}
	| '+' 		{PLUS}
	| '-' '>' 	{ARROW}
	| '-'		{MINUS}
	| '*'		{STAR}
	| '/'		{DIV}
	| '%'		{MOD}
	| '='		{EQUAL}
	| '('		{LP}
	| ')'		{RP}
	| '['		{LSQ}
	| ']'		{RSQ}
	| '{'		{LB}
	| '}'		{RB}
	| ','		{COMMA}
	| '.'		{POINT}
	| ':'		{COLON}
	| ';'		{SEMICOLON}
	| '\n' {new_line lexbuf; token lexbuf}	(* retour chariot *)
	| '"' 		{STRING(find_string lexbuf)}

	| (space|cmnt)+ {token lexbuf}		(* ignorer espace + commentaire *)
	| "/*" {comment lexbuf; token lexbuf}	(* commentaires imbriques *)
	| cmnt eof 								(* commentaire fin fichier *)
	| eof { EOF }							(* end of file *)
	|_ as c {raise (Lexing_error ("illegal character " ^ String.make 1 c))}



(* commentaires imbriques*)
and comment = parse
	| "*/" { () }
	| "/*" { comment lexbuf; comment lexbuf }
	| '\n' {new_line lexbuf; comment lexbuf}
	| _ { comment lexbuf }
	| eof { raise (Lexing_error ("unterminated comment")) }


(* chaines de caracteres dans print!*)
and find_string = parse
	| '"' {let s =Buffer.contents string_buffer in
			Buffer.reset string_buffer;
			s}
	| "\\\"" {Buffer.add_char string_buffer '"';
			  find_string lexbuf}
	| "\\n" {Buffer.add_char string_buffer '\n';
			 find_string lexbuf}
	| "\\\\" {Buffer.add_char string_buffer '\\';
			  find_string lexbuf}

	| _ as c {Buffer.add_char string_buffer c;
			  find_string lexbuf}
	| eof { raise(Lexing_error ("unterminated string")) }

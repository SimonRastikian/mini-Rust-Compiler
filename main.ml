
(* Programme principal *)
open Format
open Lexing

let usage = "usage: mini-rust [options] file.rs"

let parse_only = ref false
let type_only = ref false
let no_asm = ref false

let options =
  [ "--parse-only", Arg.Set parse_only, "  stop after parsing";
    "--type-only", Arg.Set type_only, " simple typing";
    "--no-asm", Arg.Set no_asm, " stop before code production"]

(* Load the file *)
let load_file =
  let f = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".rs") then
      raise (Arg.Bad "no .rs extension");
      f := Some s
  in
  Arg.parse options set_file usage;
  match !f with
  | Some f -> f
  | None -> Arg.usage options usage;
            exit 1

(* Print the line and characters numbers of the error *)
let print_error_position filename start_p end_p =
  let line = start_p.pos_lnum in
  let cs = start_p.pos_cnum - start_p.pos_bol + 1 in
  let ce = end_p.pos_cnum - start_p.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" filename line cs ce

let () =
  let filename = load_file in
  (* create a reading channel *)
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  try
    (* start parsing at the lexing buffer *)
    let parsed = Parser.parse_file Lexer.token lexbuf in
    close_in channel;
    if !parse_only then exit 0;
    (* start typing the parsed file *)
    let typed = Typer.type_file parsed in
    if !type_only then exit 0;
    if !no_asm then exit 0;
    (* start compiling the typed file *)
    let out_name = (Filename.chop_suffix filename ".rs") ^ ".s" in
    Compile.compile_file typed out_name
  with
  | Lexer.Lexing_error str ->
  (* catch the lexing problem *)
	eprintf "Syntax Error: %s@." str;
	print_error_position filename (lexeme_start_p lexbuf)  (lexeme_end_p lexbuf);
	exit 1
  | Parser.Error ->
  (* cannot catch the exact parsing problem *)
	eprintf "Syntax Error. @.";
	print_error_position filename (lexeme_start_p lexbuf) (lexeme_end_p lexbuf);
	exit 1
  | TypCheck.Typing_error str ->
  (* catch the typing problem *)
	eprintf "Type Error: %s@." str;
	exit 1
  | e ->
  (* some random anomaly detected *)
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2



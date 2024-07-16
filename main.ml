
(* Programme principal *)

open Format
open Lexing
open Parser

let usage = "usage: mini-rust [options] file.rs"

let parse_only = ref false
let type_only = ref false
let no_asm = ref false

let options =
  [ "--parse-only", Arg.Set parse_only, "  stop after parsing";
    "--type-only", Arg.Set type_only, " simple typing";
    "--no-asm", Arg.Set no_asm, " stop before code production"]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".rs") then
      raise (Arg.Bad "no .rs extension");
    file := Some s
  in
  Arg.parse options set_file usage;
  match !file with Some f -> f | None -> Arg.usage options usage; exit 1

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let filename = file in
  let c = open_in filename in
  let lb = Lexing.from_channel c in
  try
    let p = Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    let tf = Typer.type_file p in
    if !type_only then exit 0;
    if !no_asm then exit 0;  
    let out_name = (Filename.chop_suffix filename ".rs") ^ ".s" in
    Compile.compile_program tf out_name
  with
    | Lexer.Lexing_error s ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s@." s;
	exit 1
    | Parser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error@.";
	exit 1
    | TypCheck.Typing_error s ->
	eprintf "File \"%s\", %s@." filename s;
	exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2



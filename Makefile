CMO=lexer.cmo parser.cmo att.cmo typCheck.cmo typer.cmo x86_64.cmo compile.cmo main.cmo
GENERATED = lexer.ml parser.ml parser.mli
FLAGS=-annot -g



mini_rustc: $(CMO)
	ocamlc $(FLAGS) -o $@ $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir --infer -v $<

.mly.mli:
	menhir  -v $<

clean:
	rm -f *.cm[io] *.o *.annot $(GENERATED)
	rm -f parser.output parser.automaton parser.conflicts
	rm -f .depend a.out
	rm -f lexer.ml parser.ml mini_rustc
	rm -rf *.s

parser.ml: ast.cmi

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend

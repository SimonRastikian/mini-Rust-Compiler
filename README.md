# About mini-Rust
mini-Rust was implemented at Ecole Normale Superieure (ENS Paris)
by S. Rastikian and M. Janin. The specifications are given by
Prof. Jean-Christophe Filliâtre as part of his course project.
The implementation was originally done in 2018 and is being revisited currently.

## Installation
Install Ocaml and Menhir

## Compilation and Execution
To compile the code type
``` bash
make
```
Once compiled the Makefile will generate our lovely compiler mini_rustc.

To run the tests use the command.
``` bash
chmod -x run-tests
./run-tests <option> ./mini_rustc <option>
```

For example to run all the test cases
``` bash
./run-tests -all ./mini_rustc
```

## Organization
Main calls the Lexer then the Parser on the file.
If no errors were detected it calls the Typer and the BorrowChecker.
It finally compiles producing an assembly code for X86-64 Assembly.
The assembly can be executed on your machine.

### On the Syntax Checking
The Syntax checking is provided by the
- lexer.mll: reads the code and transforms the corresponding characters into constructors
- ast.mli: the abstract syntax tree that provides the necessary constuctors for Rust syntax
- parser.mly: the parser that fixes the rules over which the parsing will succeed or fail


### On the Type Checking
- typer.ml: transforms the parser's ast constructor into the typer's att constructors
- att.ml: the abstract type tree that provides the necessary constructors for Rust types
- typCheck.ml: checks if the types are matching


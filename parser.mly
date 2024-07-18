%{
  open Ast
%}
/* tokens  */
%token <Ast.constant> CST
%token <Ast.binop> CMP
%token <string> IDENT
%token <string> STRING
%token ELSE FN IF LET RETURN STRUCT WHILE
%token LEN VEC EOF PRINT
%token ARROW EQUAL BLT BGT
%token PLUS MINUS STAR DIV MOD
%token AND OR MUT BORR NOT
%token LP RP LSQ RSQ COMMA POINT COLON SEMICOLON LB RB

/* ordre de priorite */
%right EQUAL
%left OR
%left AND
%nonassoc CMP BLT BGT
%left PLUS MINUS
%left STAR DIV MOD
%nonassoc NOT unary_minus unary_star BORR MUT
%nonassoc LSQ
%nonassoc POINT


/* Start the parser at the file constructor */
%start file
%type <Ast.file> file

%%
// a file is a list of declarations that ends with EOF
file:
	d = list(decl) EOF
	{d}
;

// a declaration is either a function or a structure
decl:
| FN f = decl_fun
	{Dfun f}
| STRUCT d = decl_struct
	{Dstruct d}
;

// a structure is an identifier, `{`, body, `}`
decl_struct:
	name = ident
	LB
	// the body is a sequence of identifiers:value ending with comma
	body = separated_list(COMMA,separated_pair (ident,COLON,typ))
	RB
	{ { name = name;
	    body = body;
	    localisation = $startpos,$endpos} }
;

// a function is a name `(` arguments `)` -> type {...}
decl_fun:
	name = ident
	LP
	// arguments are comma separated
	formals = separated_list(COMMA,arg)
	RP
	// declaring the function type in rust is optional
	t = option(preceded(ARROW,typ))
	body = block
	{ { name = name;
		formals = (formals,t);
		body = body;
		localisation = $startpos,$endpos } }
;

// an argument is mut identifier: type
arg:
	// the mut is optional
	mut = boption(MUT) i = ident COLON t=typ
	{ (mut,i),t }
;

// a type could have several syntax
typ:
	t = my_type
	{ {my_type = t;
	  localisation = $startpos,$endpos} }
;

my_type:
| i=ident									// identifier
	{ Tident i }
| i=ident BLT t=typ BGT		// <identifier>
	{ Tidtyp (i,t) }
| BORR t = typ						// &typ
	{ Tref t }
| BORR MUT t = typ				// &mut typ
	{ Trefmut t }
;

// a block body ...
blockbody :
| e=option(expr)					// empty or contains expressions
	{ ([],e) }
| s=stmt b=blockbody			// statement ...
	{ (s::(fst b),snd b) }
;

// a block {...}
block:
| LB body=blockbody RB
	{body}
;

stmt:
| SEMICOLON							// ;
	{ Snone }
| e = expr SEMICOLON		// exp;
	{ Sexpr e }
| LET boolean = boption(MUT) name = ident EQUAL e = expr SEMICOLON // let mut identifier = exp;
	{ Sletv((boolean,name),e) }
| LET boolean = boption(MUT) name = ident EQUAL i = ident LB
  l = separated_list(COMMA,separated_pair(ident, COLON, expr)) RB SEMICOLON
  	{ Slets((boolean,name),i,l) }			// let mut identifier = identifier {identifier:exp, ...};
| WHILE e = expr b = block						// while e {...}
	{ Swhile(e,b) }
| RETURN e = option(expr) SEMICOLON		// return e;
	{ Sret e }
| IF e = ifcmp				// if
	{ Sif e }
;

expression:
| c = CST
	{ Ecst c }
| i = ident
	{ Eident i }
| MINUS e1 = expr %prec unary_minus // -e
  { Eunop(Uneg,e1) }
| STAR e1 = expr %prec unary_star		// *e
	{ Eunop(Uderef,e1) }
| u=unary e1=expr
	{ Eunop(u,e1) }
| e1 = expr o = binop  e2 = expr
	{ Ebinop(o,e1,e2) }
| e=expr POINT i=ident
	{ Estruct(e,i) }
| e=expr POINT LEN LP RP
	{ Elen e }
| e1=expr LSQ e2=expr RSQ
	{ Eget(e1,e2) }
| i=ident LP le=separated_list(COMMA,expr) RP
	{ Efun(i,le) }
| VEC NOT LSQ le=separated_list(COMMA,expr) RSQ
	{ Evect le }
| PRINT NOT LP s=STRING RP
	{ Eprint s }
| b=block
	{ Eblock b }
| LP e=expr RP
	{ Eparenthese e }
;

expr:
| d=expression
	{ {expression = d;
	  localisation = $startpos,$endpos} }
;

ifcmp:
| e=expr b=block t=option(preceded(ELSE,elsecmp))
	{e,b,t}
;

elsecmp:
| b = block
	{Cblock b}
| IF i = ifcmp
	{Cif i}
;

%inline binop:
| PLUS  { Badd }
| MINUS { Bsub }
| STAR  { Bmul }
| DIV   { Bdiv }
| MOD   { Bmod }
| c=CMP { c }
| BLT		{ Blt }
| BGT		{ Bgt }
| AND   { Band }
| OR    { Bor }
| EQUAL { Baff }
;

%inline unary:
| NOT  		 {Unot}
| BORR MUT {Umut}
| BORR		 {Uborr}
;

ident:
 | id = IDENT {id}
 | PRINT {"print"}
 | VEC {"vec"}
;

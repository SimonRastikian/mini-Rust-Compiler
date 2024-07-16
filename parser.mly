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


/* entree de la grammaire et type de valeurs renvoyees */
%start file
%type <Ast.fichier> file

%%
file:
| d = list(decl) EOF 
	{d}
;

decl:
| FN f = decl_fun 
	{Dfun f}
| STRUCT d = decl_struct
	{Dstruct d}
;

decl_struct:
	name = ident
	LB
	body = separated_list(COMMA,separated_pair (ident,COLON,typ))
	RB
	{ { name = name;
	    body = body;
	    localisation = $startpos,$endpos} }
;

decl_fun:
	name = ident
	LP
	formals = separated_list(COMMA,arg)
	RP
	t= option (preceded(ARROW,typ))
	body = bloc
	{ { name = name;
		formals = (formals,t);
		body = body;
		localisation = $startpos,$endpos } }
;

arg:
| mut = boption(MUT) i = ident COLON t=typ
	{ (mut,i),t }
;

typ:
| t = my_type 
	{ {my_type = t;
	  localisation = $startpos,$endpos} }
;

my_type:
| i=ident 
	{ Tident i }
| i=ident BLT t=typ BGT
	{ Tidtyp (i,t) }
| BORR t = typ
	{ Tref t } 
| BORR MUT t = typ
	{ Trefmut t }
;

blocbody :
| e=option(expr)
	{ ([],e) }
| s=stmt b=blocbody   
	{ (s::(fst b),snd b) }
;

bloc:
| LB body=blocbody RB 
	{body}
;

stmt:
| SEMICOLON
	{ Snone }
| e = expr SEMICOLON
	{ Sexpr e }
| LET boolean = boption(MUT) name = ident EQUAL e = expr SEMICOLON
	{ Sletv((boolean,name),e) }
| LET boolean = boption(MUT) name = ident EQUAL i = ident LB
  l = separated_list(COMMA,separated_pair(ident, COLON, expr)) RB SEMICOLON
  	{ Slets((boolean,name),i,l) }
| WHILE e = expr b = bloc
	{ Swhile(e,b) }
| RETURN e = option(expr) SEMICOLON
	{ Sret e }
| IF e = ifcmp 
	{ Sif e }
;

expression:
| c = CST 
	{ Ecst c } 
| i = ident
	{ Eident i }
| MINUS e1 = expr %prec unary_minus
    { Eunop (Uneg,e1) }
| STAR e1 = expr %prec unary_star
	{ Eunop (Uderef,e1)}
| u=unary e1=expr
	{ Eunop (u,e1)}
| e1 = expr o = binop  e2 = expr
	{ Ebinop(o,e1,e2) } 
| e=expr POINT i=ident 
	{Estruct (e,i)}
| e=expr POINT LEN LP RP
	{Elen e}
| e1=expr LSQ e2=expr RSQ
	{Eget(e1,e2)}
| i=ident LP le=separated_list(COMMA,expr) RP
	{Efun(i,le)}
| VEC NOT LSQ le=separated_list(COMMA,expr) RSQ
	{Evect le}
| PRINT NOT LP s=STRING RP
	{Eprint s}
| b=bloc
	{Ebloc b}
| LP e=expr RP
	{Eparenthese e}
;

expr:
| d=expression 
	{ {expression = d;
	  localisation = $startpos,$endpos} }
;

ifcmp:
| e=expr b=bloc t=option(preceded(ELSE,elsecmp))
	{e,b,t}
;

elsecmp:
| b = bloc
	{Cbloc b}
| IF i = ifcmp
	{Cif i}
;

%inline binop:
| PLUS  { Badd }
| MINUS { Bsub }
| STAR  { Bmul }
| DIV   { Bdiv }
| MOD   { Bmod }
| c=CMP { c    }
| BLT	{ Blt  }
| BGT	{ Bgt  }
| AND   { Band }
| OR    { Bor  }
| EQUAL { Baff }
;

%inline unary:
| NOT  		 {Unot}
| BORR MUT   {Umut}
| BORR		 {Uborr}
;

ident:
 | id = IDENT {id}
 | PRINT {"print"}
 | VEC {"vec"}
; 

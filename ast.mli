type ident = string                (* variable *)
type mut = bool * ident            (* mutable ou non *)

type unop =
  | Uneg            (* -e *)
  | Unot            (* !e *)
  | Uderef          (* *e *)
  | Umut            (* &mut e *)
  | Uborr           (* &e *)

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor | Baff                   (* && || = *)

type constant =
  | Cbool of bool
  | Cint of int

(* Lexing.position helps detect the exact
  line number and char number errors in Rust code *)
type expr =
{ expression : expression;
  localisation : Lexing.position*Lexing.position }

and expression =
  | Ecst of constant                  (* integers and booleans *)
  | Eident of ident                   (* identifiers: var, func, keywords... *)
  | Ebinop of binop * expr * expr     (* binary operations on two expressions *)
  | Eunop of unop * expr              (* unary operations on one expression*)
  | Estruct of expr * ident           (* e1.x *)
  | Elen of expr                      (* e1.len() *)
  | Eget of expr * expr               (* e1[e2] *)
  | Efun of ident * expr list         (* f(e1,e2) *)
  | Evect of expr list                (* vec![e1,e2] *)
  | Eprint of string                  (* print!() *)
  | Eblock of block                   (* { statements * potential expressions } *)
  | Eparenthese of expr               (* (e1) *)

and block = stmt list  * expr option         (* { stmt and expr(?) } *)

and stmt =
  | Snone                                       (* ; *)
  | Sexpr of expr                               (* e1; *)
  | Sletv  of mut * expr                        (* let mut x=e1; *)
  | Slets of mut * ident * (ident * expr) list  (* let mut x=y{a=e1,b=e2}; *)
  | Swhile of expr * block                       (* while ;*)
  | Sret of expr option                         (* return ; *)
  | Sif of ifcmp                                (* if else; *)

and ifcmp = expr * block * elsecmp option    (* if expr {...} else(?) )*)

and elsecmp =
  | Cblock of block (* else {...} *)
  | Cif of ifcmp  (* else if ... )*)

type typ =
{  my_type : my_type;
  localisation : Lexing.position*Lexing.position }

and my_type=
  | Tident of ident                         (* ident *)
  | Tidtyp of ident * typ                   (* ident type *)
  | Tref of typ                             (* &type *)
  | Trefmut of typ                          (* &mut type *)

type arg = mut * typ                        (* mut x : type *)

type decl_fun =
  { (* fn (a,b) {} *)
    name : ident;
    formals : arg list * typ option;
    body: block;
    localisation : Lexing.position*Lexing.position
  }

type decl_struct =
  { (* struct x {y:type} *)
    name : ident ;
    body : (ident * typ) list;
    localisation : Lexing.position*Lexing.position
  }

type decl =
  | Dfun of decl_fun          (* function *)
  | Dstruct of decl_struct    (* structure *)

type sfile = decl list


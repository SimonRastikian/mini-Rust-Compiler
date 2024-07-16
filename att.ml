type loc = Lexing.position * Lexing.position
module Decls = Map.Make(String)

type borrow = {
   borr_mut : bool;
   borr_typ : typ;
}

and typ =
   | Tunit
   | Ti32
   | Tbool
   | Tstruct of Ast.ident
   | Tvect of typ
   | Tborr of borrow
   | Alpha of ((typ option) ref) ref
   | Ret

type texpr = {
   te_expr : texpression;
   te_localisation : Lexing.position * Lexing.position;
   te_typ : typ;
   te_lval : bool;
   te_mut : bool;
}

and texpression =
   | Ecst of Ast.constant
   | Eident of Ast.ident
   | Ebinop of Ast.binop * texpr * texpr
   | Eunop of Ast.unop * texpr
   | Elen of texpr
   | Egetv of texpr * texpr
   | Egets of texpr * Ast.ident
   | Efun of Ast.ident * texpr list
   | Evect of vector
   | Eprint of string
   | Ebloc of tbloc
   
   | Enone
   | Eletv of bool * Ast.ident * texpr
   | Elets of bool * Ast.ident * Ast.ident * texpr Decls.t
   | Ewhile of texpr * tbloc
   | Eret of texpr
   | Eif of tifcmp

and tbloc = texpr list
and tifcmp = {
   cond : texpr;
   then_bloc : tbloc;
   else_bloc : tbloc;
}

and vector = { arr : texpr array ;
                len : int }

type var_decl = {
   vdecl_mut : bool;
   vdecl_typ : typ;
}

type struct_decl = typ Decls.t

type fun_decl = {
   fdecl_args : (Ast.ident * var_decl) list;
   fdecl_rty : typ;
}

type var_decls = var_decl Decls.t
type struct_decls = struct_decl Decls.t
type fun_decls = fun_decl Decls.t

type env = {
   env_vars : var_decls;
   env_structs : struct_decls;
   env_funs : fun_decls;
   env_rty : typ;
}

type fun_def = {
   decl : fun_decl;
   args : Ast.ident list;
   body : tbloc
}

type fun_defs = fun_def Decls.t

type tfile = {
   file_structs : struct_decls;
   file_funs : fun_defs;
   struct_order : Ast.ident list;
   fun_order :  Ast.ident list;
}

let empty_env = {
   env_vars = Decls.empty;
   env_structs = Decls.empty;
   env_funs = Decls.empty;
   env_rty = Tunit;
}

let new_alpha () = ref (ref None)
let new_beta () = Tborr { borr_mut = false ; borr_typ = Alpha (new_alpha ()) }
let sigma = ""
let zero_pos =
   { Lexing.pos_fname = "fily mac fileface" ;
     Lexing.pos_lnum = 0 ;
     Lexing.pos_bol = 0 ;
     Lexing.pos_cnum = 0 ;
   }
let zero_loc = (zero_pos, zero_pos)

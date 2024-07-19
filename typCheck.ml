open Att
open Printf

exception Typing_error of string

module IdSet = Set.Make(String)

(* Auxiliary functions *)

let typ_error (b, e) str =
   let open Lexing in
   assert (b.pos_fname = e.pos_fname);
   assert (b.pos_cnum <= e.pos_cnum);
   let line = string_of_int b.pos_lnum in
   let scharac = b.pos_cnum - b.pos_bol in
   let echarac = e.pos_cnum - e.pos_bol in
   raise (Typing_error(
      sprintf
      "line %s, characters %d-%d:\ntype error : %s.\n"
      line scharac echarac str
      ))

let lval loc b =
   if not b then typ_error loc "This variable is not a l-value"

let mut loc b =
   if not b then typ_error loc "This variable is not mutable"


(* Used for transforming a type into a string for error messages *)
let string_of_typ = function
   | Tbool        -> "bool"
   | Ti32         -> "int32"
   | Tunit        -> "unit"
   | Tstruct id   -> "struct " ^ id
   | Tborr b      -> "'a borrow"
   | Tvect t      -> "'a vect"
   | _            -> assert false

let vect_decl env loc s =
   if (not (s = "Vec") || Decls.mem "Vec" env.env_structs) then
      typ_error loc "The only allowed parametrized type is Vec"

let get_thing decls loc i str =
   try Decls.find i decls
   with Not_found -> typ_error loc (sprintf "%s %s" str i)

let get_struct_decl env loc si =
   get_thing env.env_structs loc si "Unkown structure identifier"

let get_fun_decl env loc fi =
   get_thing env.env_funs loc fi "Unkown function identifier"

let rec typ_decl env borrow_ok current_si under_vect ast_typ =
   match ast_typ.Ast.my_type with
   | Ast.Tident id when id = current_si   ->
         if under_vect then () else
            typ_error
               ast_typ.Ast.localisation
               (sprintf "Struct %s Recursively contains itself" id)
   | Ast.Tident id when id = "()"    -> ()
   | Ast.Tident id when id = "bool"  -> ()
   | Ast.Tident id when id = "i32"   -> ()
   | Ast.Tident id              ->
         ignore
         (get_thing
            env.env_structs
            ast_typ.Ast.localisation
            id
            "Unkown structure identifier"
         )
   | Ast.Tidtyp (id, ast_typ)  ->
         vect_decl env ast_typ.Ast.localisation id;
         typ_decl env borrow_ok current_si true ast_typ
   | Ast.Tref ast_typ | Ast.Trefmut ast_typ     ->
         if borrow_ok then
            typ_decl env borrow_ok current_si under_vect ast_typ
         else
            typ_error ast_typ.Ast.localisation "Cannot contain borrow"

let rec uniq loc set = function
   | []     -> ()
   | h :: t ->
         if IdSet.mem h set then
            typ_error loc ("identifier " ^ h ^ " is defined twice")
         else
            uniq loc (IdSet.add h set) t

let uniql loc l = uniq loc IdSet.empty l

(*
typ function checks whether t1 and t2 have corresponding types
*)
let rec typ loc t1 t2 =
   match t1, t2 with
   | _, Ret -> ()
   | Ret, _ -> assert false
   | Alpha r1, Alpha r2 -> (* alpha is a double reference to a type*)
      (match !(!r1), !(!r2) with
      | None, None -> r1 := !r2
      | Some t, None | None, Some t -> !r1 := Some t
      | Some t1, Some t2 -> typ loc t1 t2)
   | Alpha r, t ->
      (match !(!r) with
      | None -> !r := Some t
      | Some t' -> typ loc t' t)
   | t, Alpha r ->
      (match !(!r) with
      | None -> !r := Some t
      | Some t' -> typ loc t t')
   | Tborr borr1, Tborr borr2 ->
      typ loc borr1.borr_typ borr2.borr_typ;
      if (not borr2.borr_mut && borr1.borr_mut) then
         typ_error loc
         "This expression is not mutable as it is expected to be"
   | Tvect t1, Tvect t2 -> typ loc t1 t2
   | Tstruct "", Tstruct i | Tstruct i, Tstruct "" -> ()
   | t1, t2 ->
      if not (t1 = t2) then
         typ_error loc
            (sprintf
            ( "This expression has type %s " ^^
               "but an expression was expected of type %s")
            (string_of_typ t2)
            (string_of_typ t1))
let get_var env loc i =
   get_thing env.env_vars loc i "Unkown variable identifier"

let get_field env loc si i =
   let struct_decl = get_struct_decl env loc si in
   get_thing struct_decl loc i "Unkown structure field"

let get_f env loc fi tel =
   let fun_decl = get_fun_decl env loc fi in
   let rec check_args count var_decll tel =
      match var_decll, tel with
      | var_decl :: t1, te :: t2 ->
            typ te.te_localisation var_decl.vdecl_typ te.te_typ;
            check_args (count + 1) t1 t2
      | [], []                   -> ()
      | [], h :: t               ->
            typ_error loc
            ( sprintf
              ( "To many arguments. This function takes %d arguments " ^^
                "but %d where given" )
              count
              (count + List.length tel)
            )
      | h :: t, []               ->
            typ_error loc
            ( sprintf
              ( "To few arguments. This function takes %d arguments " ^^
                "but %d where given" )
              (count + List.length var_decll)
              count
            )
   in
   check_args 0 (List.map snd fun_decl.fdecl_args) tel;
   fun_decl.fdecl_rty

let get_s env loc si decll =
   let struct_decl = get_struct_decl env loc si in
   let expected_fields =
      (*  IdSet.of_list (List.map fst (Decls.bindings struct_decl))
         Cette fonction a ete change parceque ca ne marche pas chez moi*)
      List.fold_left
         (fun set elem -> IdSet.add elem set)
               IdSet.empty (List.map fst (Decls.bindings struct_decl))

      in
   let rec check_fields expectf decll =
      match expectf, decll with
      | set, (id, te) :: t ->
            let expected_typ = get_field env loc si id in
            typ te.te_localisation expected_typ te.te_typ;
            check_fields (IdSet.remove id set) t
      | set, [] ->
            if not (IdSet.is_empty set) then
               typ_error loc
               ( sprintf
                 "The following field are not provided :%s"
                 (IdSet.fold (fun id str -> sprintf "\n%s%s" id str) set "")
               )
   in
   check_fields expected_fields decll

let main decl loc =
   if not (decl.fdecl_args = []) then
      typ_error loc "Function main must not take argument"
   else
   if not (decl.fdecl_rty = Tunit) then
      typ_error loc "Function main must not return something"
   else ()

let file tfile =
   if not (Decls.mem "main" tfile.file_funs) then
      typ_error zero_loc "Function main not found"

let arr_typ env arr =
   let len = Array.length arr in
   assert (len != 0);
   let t = ref (snd arr.(0)) in
   let update_typ loc_typ =
      try
         typ zero_loc (snd loc_typ) !t;
         t := (snd loc_typ)
      with Typing_error _ ->
         typ (fst loc_typ) !t (snd loc_typ)
   in
   Array.iter update_typ arr;
   !t
open Att

(* Auxiliary functions *)

let convert_idtyp env i =
if Decls.mem i env.env_structs then Tstruct i else
if i = "i32" then Ti32 else
if i = "bool" then Tbool else
if i = "()" then Tunit else
Tstruct i

let rec convert_typ env ast_typ =
   match ast_typ.Ast.my_type with
   | Ast.Tident i             -> convert_idtyp env i
   | Ast.Tidtyp (i, ast_typ)  ->
         let () = TypCheck.vect_decl env ast_typ.Ast.localisation i in
         Tvect (convert_typ env ast_typ)
   | Ast.Tref ast_typ         ->
         Tborr { borr_mut = false ; borr_typ = convert_typ env ast_typ }
   | Ast.Trefmut ast_typ      ->
         Tborr { borr_mut = true ; borr_typ = convert_typ env ast_typ }

let check_desopt_typ env struct_loc opt_typ =
   match opt_typ with
   | None     -> struct_loc, Tunit
   | Some ast_typ ->
         let () = TypCheck.typ_decl env false "" false ast_typ in
         ast_typ.Ast.localisation, convert_typ env ast_typ

let deref te =
   match te.te_typ with
   | Tborr { borr_typ = Tstruct t ; borr_mut } ->
         { te with
           te_typ = Tstruct t ;
           te_expr = Eunop (Ast.Uderef, te) ;
           te_mut = borr_mut ;
         }
   | Tborr { borr_typ = Tvect t ; borr_mut }   ->
         { te with
           te_typ = Tvect t ;
           te_expr = Eunop (Ast.Uderef, te) ;
           te_mut = borr_mut ;
         }
   | _                                  -> te

let purify env il =
   let funs = env.env_funs in
   let purified_funs = List.fold_right Decls.remove il funs in
   { env with env_funs = purified_funs }

(* Typer *)

let type_binop env localisation binop te1 te2 =
   match binop with
   | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv | Ast.Bmod ->
         let () = TypCheck.typ te1.te_localisation Ti32 te1.te_typ in
         let () = TypCheck.typ te2.te_localisation Ti32 te2.te_typ in
         { te_expr   = Ebinop (binop, te1, te2) ;
           te_typ          = Ti32 ;
           te_lval         = false ;
           te_mut          = false;
           te_localisation = localisation ;
         }
   | Ast.Beq | Ast.Bneq | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge ->
         let () = TypCheck.typ te1.te_localisation Ti32 te1.te_typ in
         let () = TypCheck.typ te2.te_localisation Ti32 te2.te_typ in
         { te_expr   = Ebinop (binop, te1, te2) ;
           te_typ          = Tbool ;
           te_lval         = false ;
           te_mut          = false ;
           te_localisation = localisation ;
         }
   | Ast.Band | Ast.Bor ->
         let () = TypCheck.typ te1.te_localisation Tbool te1.te_typ in
         let () = TypCheck.typ te2.te_localisation Tbool te2.te_typ in
         { te_expr   = Ebinop (binop, te1, te2) ;
           te_typ          = Tbool ;
           te_lval         = false ;
           te_mut          = false ;
           te_localisation = localisation ;
         }
   | Ast.Baff ->
         let () = TypCheck.typ te2.te_localisation te1.te_typ te2.te_typ in
         let () = TypCheck.lval te1.te_localisation te1.te_lval in
         let () = TypCheck.mut te1.te_localisation te1.te_mut in
         { te_expr   = Ebinop (binop, te1, te2) ;
           te_typ          = Tunit ;
           te_lval         = false ;
           te_mut          = false ;
           te_localisation = localisation ;
         }

let type_unop env localisation unop te =
   match unop with
   | Ast.Uneg ->
         let () = TypCheck.typ te.te_localisation Ti32 te.te_typ in
         { te_expr   = Eunop (unop, te) ;
           te_typ          = Ti32 ;
           te_lval         = false ;
           te_mut          = false ;
           te_localisation = localisation ;
         }
   | Ast.Unot ->
         let () = TypCheck.typ te.te_localisation Tbool te.te_typ in
         { te_expr   = Eunop (unop, te) ;
           te_typ          = Tbool ;
           te_lval         = false ;
           te_mut          = false ;
           te_localisation = localisation ;
         }
   | Ast.Uderef ->
         let () =
            TypCheck.typ te.te_localisation (new_beta ()) te.te_typ
         in
         let t, m =
            match te.te_typ with
            | Tborr { borr_typ ; borr_mut }  -> borr_typ, borr_mut
            | _                              -> assert false
         in
         { te_expr   = Eunop (unop, te) ;
           te_typ          = t ;
           te_lval         = true ;
           te_mut          = m ;
           te_localisation = localisation ;
         }
   | Ast.Umut ->
         let () = TypCheck.lval te.te_localisation te.te_lval in
         let () = TypCheck.mut te.te_localisation te.te_mut in
         { te_expr   = Eunop (unop, te) ;
           te_typ = Tborr { borr_mut = true ; borr_typ = te.te_typ } ;
           te_lval = false ;
           te_mut = false ;
           te_localisation = localisation ;
         }
   | Ast.Uborr ->
         let () = TypCheck.lval te.te_localisation te.te_lval in
         { te_expr   = Eunop (unop, te) ;
           te_typ = Tborr { borr_mut = false ; borr_typ = te.te_typ } ;
           te_lval = false ;
           te_mut = false ;
           te_localisation = localisation ;
         }

let rec type_stmt env stmt =
   match stmt with
   | Ast.Snone ->
         (
            { te_expr         = Enone ;
              te_typ          = Tunit ;
              te_lval         = false ;
              te_mut          = false ;
              te_localisation = zero_loc ;
            },
            env,
            false
         )
   | Ast.Sexpr e ->
         let te, always_return = type_expr env e in
         (te, env, always_return)
   | Ast.Sletv ((m, i), e) ->
         let te, always_return = type_expr env e in
         let var_decl = { vdecl_mut = m ; vdecl_typ = te.te_typ } in
         let new_env =
            { env with env_vars = Decls.add i var_decl env.env_vars }
         in
         let new_env = purify new_env [i] in
         (
            { te_expr         = Eletv (m, i, te) ;
              te_typ          = Tunit ;
              te_lval         = false ;
              te_mut          = false ;
              te_localisation = zero_loc ;
            },
            new_env,
            always_return
         )
   | Ast.Slets ((m, i), si, fl) ->
         let il = List.map fst fl in
         let el = List.map snd fl in
         let te_always_returnl = List.map (type_expr env) el in
         let tel = List.map fst te_always_returnl in
         let decll = List.map2 (fun x y -> (x, y)) il tel in
         let () = TypCheck.get_s env zero_loc si decll in

         let var_decl = { vdecl_mut = m ; vdecl_typ = Tstruct si ; } in
         let struct_inst =
            List.fold_left2
            (fun d x y -> Decls.add x y d)
            Decls.empty
            il tel
         in
         let new_env =
            { env with env_vars = Decls.add i var_decl env.env_vars }
         in
         let new_env = purify new_env [i] in
         (
            { te_expr         = Elets (m, i, si, struct_inst) ;
              te_typ          = Tunit ;
              te_lval         = false ;
              te_mut          = false ;
              te_localisation = zero_loc ;
            },
            new_env,
            false
         )
   | Ast.Swhile (e, block) ->
         let te, always_return = type_expr env e in
         let tblock, t, _ = type_block env block in
         let () = TypCheck.typ te.te_localisation Tbool te.te_typ in
         let () = TypCheck.typ te.te_localisation Tunit t in
         (
            { te_expr   = Ewhile (te, tblock) ;
              te_typ          = t ;
              te_lval         = false ;
              te_mut          = false ;
              te_localisation = te.te_localisation ;
            },
            env,
            false
         )
   | Ast.Sret None ->
         let () = TypCheck.typ zero_loc env.env_rty Tunit in
         (
            { te_expr   = Eret { te_expr     = Enone ;
                                       te_typ            = Tunit ;
                                       te_lval           = false ;
                                       te_mut            = false ;
                                       te_localisation   = zero_loc ;
                                  } ;
              te_typ          = Ret ;
              te_lval         = false ;
              te_mut          = false ;
              te_localisation = zero_loc ;
            },
            env,
            true
         )
   | Ast.Sret Some e ->
         let te, _ = type_expr env e in
         let () = TypCheck.typ te.te_localisation env.env_rty te.te_typ in
         (
            { te_expr   = Eret te ;
              te_typ          = Ret ;
              te_lval         = false ;
              te_mut          = false ;
              te_localisation = zero_loc ;
            },
            env,
            true
         )
   | Ast.Sif (e, b, else_comp_opt) ->
         let te, _ = type_expr env e in
         let then_tblock, then_typ, then_always_return = type_block env b in
         let else_tblock, else_typ, else_always_return =
            match else_comp_opt with
            | None                   -> ([], Tunit, false)
            | Some (Ast.Cblock b)     -> type_block env b
            | Some (Ast.Cif ifcomp)  -> type_block env ([Ast.Sif ifcomp], None)
         in
         let tifcomp =
            { cond = te ;
              then_block = then_tblock ;
              else_block = else_tblock ;
            }
         in
         let () = TypCheck.typ te.te_localisation Tbool te.te_typ in
         let () = TypCheck.typ zero_loc then_typ else_typ in
         (
            { te_expr   = Eif tifcomp ;
              te_typ          = then_typ ;
              te_lval         = false ;
              te_mut          = false ;
              te_localisation = zero_loc ;
            },
            env,
            then_always_return && else_always_return
         )

and type_block env block =
   match block with
   | [], None     -> ([], Tunit, false)
   | [], Some e   ->
         let te, always_return = type_expr env e in
         ([te], te.te_typ, always_return)
   | h :: t, opt  ->
         let te, new_env, always_return1  = type_stmt env h in
         let tblock, t, always_return2 = type_block new_env (t, opt) in
         (te :: tblock, t, always_return1 || always_return2)

and type_expr env { Ast.expression; Ast.localisation } =
   match expression with
   | Ast.Eblock block           ->
         let tblock, t, always_return = type_block env block in
         (
         { te_expr = Eblock tblock ;
           te_typ = t ;
           te_lval = false ;
           te_mut = false ;
           te_localisation = localisation ;
         },
         always_return
         )
   | Ast.Eparenthese e        -> type_expr env e
   | Ast.Ebinop (b, e1, e2)   ->
         let te1, always_return1 = type_expr env e1 in
         let te2, always_return2 = type_expr env e2 in
         (type_binop env localisation b te1 te2,
          always_return1 || always_return2)
   | Ast.Eunop (b, e)         ->
         let te, always_return = type_expr env e in
         (type_unop env localisation b te, always_return)
   | Ast.Ecst (Ast.Cbool b)   ->
         (
         { te_expr   = Ecst (Ast.Cbool b) ;
           te_typ          = Tbool ;
           te_lval         = false ;
           te_mut          = false ;
           te_localisation = localisation ;
         },
         false
         )
   | Ast.Ecst (Ast.Cint i)    ->
         (
         { te_expr   = Ecst (Ast.Cint i) ;
           te_typ          = Ti32 ;
           te_lval         = false ;
           te_mut          = false ;
           te_localisation = localisation ;
         },
         false
         )
   | Ast.Eident i             ->
         let var_decl = TypCheck.get_var env localisation i in
         (
         { te_expr   = Eident i ;
           te_typ          = var_decl.vdecl_typ ;
           te_lval         = true ;
           te_mut          = var_decl.vdecl_mut ;
           te_localisation = localisation ;
         },
         false
         )
   | Ast.Estruct (e, i)       ->
         let te, always_return = type_expr env e in
         let derefte = deref te in
         let () =
            TypCheck.typ te.te_localisation (Tstruct sigma) derefte.te_typ
         in
         let si =
            match derefte.te_typ with
            | Tstruct i -> i
            | _         -> assert false
         in
         let field_typ = TypCheck.get_field env localisation si i in
         (
         { te_expr         = Egets (derefte, i) ;
           te_typ          = field_typ ;
           te_lval         = true ;
           te_mut          = derefte.te_mut ;
           te_localisation = localisation ;
         },
         always_return
         )
   | Ast.Elen e               ->
         let te, always_return = type_expr env e in
         let derefte = deref te in
         let vtyp = Tvect (Alpha (new_alpha ())) in
         let () = TypCheck.typ localisation vtyp derefte.te_typ in
         (
         { te_expr         = Elen derefte ;
           te_typ          = Ti32 ;
           te_lval         = false ;
           te_mut          = false ;
           te_localisation = localisation ;
         },
         always_return
         )
   | Ast.Eget (e1, e2)        ->
         let te1, always_return1 = type_expr env e1 in
         let derefte1 = deref te1 in
         let te2,always_return2 = type_expr env e2 in
         let () = TypCheck.typ te2.te_localisation Ti32 te2.te_typ in
         let () = TypCheck.typ
            derefte1.te_localisation
            (Tvect (Alpha (new_alpha ())))
            derefte1.te_typ
         in
         let t =
            match derefte1.te_typ with
            | Tvect t -> t
            | _       -> assert false
         in
         (
         { te_expr         = Egetv (derefte1, te2) ;
           te_typ          = t ;
           te_lval         = true ;
           te_mut          = derefte1.te_mut ;
           te_localisation = localisation ;
         },
         always_return1 || always_return2
         )
   | Ast.Efun (i, el)         ->
         let te_always_returnl = List.map (type_expr env) el in
         let tel = List.map fst te_always_returnl in
         let always_returnl = List.map snd te_always_returnl in
         let always_return = List.exists (fun x -> x) always_returnl in
         let rty = TypCheck.get_f env localisation i tel in
         (
         { te_expr   = Efun (i, tel) ;
           te_typ          = rty ;
           te_lval         = false ;
           te_mut          = false ;
           te_localisation = localisation ;
         },
         always_return
         )
   | Ast.Evect []             ->
         (
         { te_expr   = Evect { arr = [||] ; len = 0 } ;
           te_typ          = Tvect (Alpha (new_alpha ())) ;
           te_lval         = false;
           te_mut          = false;
           te_localisation = localisation ;
         },
         false
         )
   | Ast.Evect l       ->
         let exp_arr = Array.of_list l in
         let te_always_return_arr = Array.map (type_expr env) exp_arr in
         let te_arr = Array.map fst te_always_return_arr in
         let always_return_arr = Array.map snd te_always_return_arr in
         (* i had to change the following function
            because it doesn't work on my version of ocaml
         let always_return =
              Array.exists (fun x -> x) always_return_arr in *)

         let always_return =
              Array.fold_left (fun x y -> x||y) false always_return_arr in
         let loc_typ_arr =
            Array.map (fun x -> (x.te_localisation, x.te_typ)) te_arr
         in
         let t = TypCheck.arr_typ env loc_typ_arr in
         (
         { te_expr         = Evect { arr = te_arr ;
                                     len = Array.length te_arr
                                   } ;
           te_typ          = Tvect t ;
           te_lval         = false;
           te_mut          = false;
           te_localisation = localisation ;
         },
         always_return
         )

   | Ast.Eprint s             ->
         (
         { te_expr   = Eprint s ;
           te_typ          = Tunit ;
           te_lval         = false ;
           te_mut          = false ;
           te_localisation = localisation ;
         },
         false
         )

let type_struct env ast_decl_struct =
   let () =
      if Decls.mem ast_decl_struct.Ast.name env.env_structs then
         TypCheck.typ_error ast_decl_struct.Ast.localisation
            ("Struct " ^ ast_decl_struct.Ast.name ^ " declared twice")
   in
   let il = List.map fst ast_decl_struct.Ast.body in
   let () = TypCheck.uniql ast_decl_struct.Ast.localisation il in

   let ast_typl = List.map snd ast_decl_struct.Ast.body in
   let () =
      List.iter
      (TypCheck.typ_decl env false ast_decl_struct.Ast.name false)
      ast_typl
   in
   let typl = List.map (convert_typ env) ast_typl in

   let struct_decl =
      List.fold_left2
      (fun d x y -> Decls.add x y d)
      Decls.empty
      il typl
   in
   (
   { env with
     env_structs =
        Decls.add ast_decl_struct.Ast.name struct_decl env.env_structs
   },
   struct_decl
   )

let type_fun env (ast_decl_fun : Ast.decl_fun) =
   let () =
      if Decls.mem ast_decl_fun.Ast.name env.env_funs then
         TypCheck.typ_error ast_decl_fun.Ast.localisation
            ("Fun " ^ ast_decl_fun.Ast.name ^ " declared twice")
   in

   let argl = fst ast_decl_fun.Ast.formals in
   let formall = List.map fst argl in

   let il = List.map snd formall in
   let () = TypCheck.uniql ast_decl_fun.Ast.localisation il in

   let ast_typl = List.map snd argl in
   let () = List.iter (TypCheck.typ_decl env true "" true) ast_typl in
   let typl = List.map (convert_typ env) ast_typl in
   let mutl = List.map fst formall in

   let ast_rty = snd ast_decl_fun.Ast.formals in
   let rty_loc, rty =
      check_desopt_typ env ast_decl_fun.Ast.localisation ast_rty
   in

   let var_decll =
      List.map2
      (fun x y -> { vdecl_mut = x ; vdecl_typ = y })
      mutl typl
   in
   let argl = List.map2 (fun x y -> (x, y)) il var_decll in
   let fun_decl =
      { fdecl_args = argl ;
        fdecl_rty = rty ;
      }
   in
   let var_decls = List.fold_right2 Decls.add il var_decll env.env_vars in
   let () =
      if ast_decl_fun.Ast.name = "main" then
         TypCheck.main fun_decl ast_decl_fun.Ast.localisation
   in

   let new_env = { env with
                   env_funs =
                     Decls.add ast_decl_fun.Ast.name fun_decl env.env_funs ;
                   env_rty = rty ;
                 }
   in
   let fun_env = { new_env with
                   env_vars = var_decls ;
                 }
   in
   let fun_env = purify fun_env il in
   let tblock, actual_rty, always_return =
      type_block fun_env ast_decl_fun.Ast.body
   in
   let () =
      if not (always_return && snd ast_decl_fun.Ast.body = None) then
         TypCheck.typ zero_loc rty actual_rty
   in

   let fun_def = { decl = fun_decl ; args = il ; body = tblock } in
   new_env, fun_def

let type_file ast_f =
   let rec aux env = function
      | []                 ->
            { file_structs = Decls.empty ;
              file_funs = Decls.empty ;
              struct_order = [] ;
              fun_order = [] ;
            }
      | Ast.Dfun d :: t    ->
            let new_env, fun_def = type_fun env d in
            let f = aux new_env t in
            let name =
               let open Ast in
               d.name
            in
            { file_structs = f.file_structs ;
              file_funs = Decls.add name fun_def f.file_funs ;
              struct_order = f.struct_order ;
              fun_order = d.Ast.name :: f.fun_order ;
            }
      | Ast.Dstruct d :: t ->
            let new_env, struct_decl = type_struct env d in
            let f = aux new_env t in
            let name =
               let open Ast in
               d.name
            in
            { file_structs = Decls.add name struct_decl f.file_structs ;
              file_funs = f.file_funs;
              struct_order = d.Ast.name :: f.struct_order ;
              fun_order = f.fun_order ;
            }
    in
    let f = aux empty_env ast_f in
    let () = TypCheck.file f in
    f

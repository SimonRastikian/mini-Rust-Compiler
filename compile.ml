open Format
open X86_64
open Att


exception VarUndef of string
let max_int = 2147483648

type exp =
  | None
  | Cst of Ast.constant	
  | Lvar of int
  | Binop of Ast.binop * exp * exp*int
  | Unop of Ast.unop * exp * int
  	(* operateur unaire expression et size de l'expression
  		utile pour le deref *)
  | Len of exp
  | Gvect of exp * exp * int
  | Gstruct of exp * int * int 
  | Call of int * Ast.ident * exp list * int
  	(* octets à dépiler (arguments), 
  	fnct à appeler , arguments, returnsize*)
  | Print of string
  | Bloc of exp list 
  	(* body *)
  | While of exp * exp list
  | If of exp * exp * exp
  | Lets of int *  (int * exp) list 
  | Letv of int * exp * int
  | Vect of exp list * int
  	(* size  *)
  | Ret of exp * int 
  	(* octets à renvoyer *)

module Smap = Map.Make(String)
type struct_typ = int * (int Smap.t)

 type file =
 	| Fun of int * string * exp list * int
 		(* size de la valeur a renvoyer 
 		nom de fonction, body, fpmax du body *) 
 	| Struct of struct_typ	
 		(* declaration de structure =
 			string du map d'en bas
 			puis le constructeur qui est 
 			la taille totale de la structure +
 			map champs taille   *) 

type local_env = int Smap.t
type map_struct = struct_typ Smap.t (* taille totale de chaque structure *)
type map_function = int Smap.t (* taille totale de chaque fonction *)
let strings = ref []
let string_ind = ref 0
let fpmax = ref 0
let envs = ref Smap.empty
let envf = ref Smap.empty

let update_fpmax v =
   fpmax := max !fpmax v


let get_size = function
   | Tstruct id -> let Struct(s, _) = Smap.find id !envs in s
   | Tvect _    -> 16
   | Tunit      -> 0
   | _          -> 8

let true_mod x y =
   let z = x mod y in
   if z < 0 then z + y else z

let rec alloc_bloc env next t =
   let el,new_next,_ = 
      List.fold_left 
      (fun (l,next,env) e ->
       let e,next,env = alloc_expr env next e in
       e::l, next,env) 
       ([],next,env) 
       t 
   in 
   let () = update_fpmax new_next in
   Bloc(List.rev el),next,env

and alloc_expr env next expr =
	match expr.te_expr with 
   | Enone -> None, next, env
	| Ecst (Cint n) -> 
         Cst (Cint ((true_mod (n + max_int) (2 * max_int)) - max_int)), 
         next,
         env
   | Ecst (Cbool b) -> Cst (Cbool b), next,env

	| Eident x -> begin try
				let ofs_x = Smap.find x env in
				Lvar ofs_x, next, env
				with Not_found -> raise (VarUndef x)
				end
	| Ebinop(op,e1,e2) ->  
	 	let size = get_size e1.te_typ in
		let e1,_, _ = alloc_expr env next e1 in
		let e2,_, _ = alloc_expr env next e2 in
		Binop(op,e1,e2,size), next,env
	| Eunop (u,e) -> 
	 	let size = get_size e.te_typ in
	 	let e,_,_ = alloc_expr env next e in
	 	Unop(u,e,size),next,env
	| Elen (e) ->
      let e, _, _ = alloc_expr env next e in
      Len(e), next, env
	| Egetv (e1,e2) -> 
      let size = get_size e2.te_typ in
		let e1,_,_ = alloc_expr env next e1 in
		let e2,_,_ = alloc_expr env next e2 in 
		Gvect(e1,e2,size), next,env
	| Egets (e,id) ->
      let Tstruct(s) = e.te_typ in
		let e,fpmax,env = alloc_expr env next e in
      let Struct(sizeret, fieldenv) = Smap.find s !envs in
      let pos = Smap.find id fieldenv in
      Gstruct(e,sizeret,-(next+pos)), next, env
	| Efun (f,l) ->
		let l =
			List.fold_left 
         (fun l e ->
         let e,_,_ = alloc_expr env next e in
         e::l)
         [] 
         l
		in
      let size = get_size expr.te_typ in
      Call(Smap.find f !envf,f,List.rev l, size),next,env 
	| Eprint (s) -> Print(s),next,env
	| Ebloc (t) -> alloc_bloc env next t
	| Ewhile (e,b) -> 
		let e1,_,_ = alloc_expr env next e in 
		let e2,_,_ = alloc_bloc env next b in
      let Bloc(b) = e2 in
		While (e1, b),next,env
	| Eret (e) -> 
      let size = get_size e.te_typ in
	 	let e,_,_= alloc_expr env next e in
      Ret(e,size),next,env
	| Eif ({cond=e;then_bloc=el1;else_bloc=el2}) ->
		let e,_,_ = alloc_expr env next e in
		let e1,_,_ = alloc_bloc env next el1 in
		let e2,_,_ = alloc_bloc env next el2 in
		If (e,e1,e2),next,env

	| Eletv (b,id,e) -> 
      let size = get_size e.te_typ in
		let e,_,_ = alloc_expr env next e in
      Letv (-next,e,size), next+size , Smap.add id (-next) env
	| Elets (b, id, sid, mapexpr) ->
		let Struct(taille,envchamps)=Smap.find sid !envs in
		let c = 
			Smap.fold 
         (fun key e l ->
               let e,_, _= alloc_expr env next e in  			  
               (Smap.find key envchamps,e)::l
         )
         mapexpr 
         [] 
      in
		Lets (-next,c),next+taille,Smap.add id (-next) env

	| Evect ({arr=a;len=elemnum}) ->
		let l = 
			Array.fold_left 
         (fun l e -> 
          let e,_,_ = alloc_expr env next e in
          e::l) 
          [] 
          a
		in
      if Array.length a = 0 then 
         Vect([], 666),next,env 
      else
         let size = get_size a.(0).te_typ in
         Vect(List.rev l, size),next,env

let alloc_structure s champs = 
	let total, envchamps = 
		Smap.fold
      (fun key typ (t,fieldenv) ->
      let size = get_size typ in
      (t + size, Smap.add key (t) fieldenv))
      champs 
      (0,Smap.empty)
	in
	let res = Struct(total,envchamps) in
	envs := Smap.add s res !envs

let alloc_function f e =
   let () = fpmax := 0 in
	let env,sizearg = 
      List.fold_left
      (fun (env,next) (name,typ)-> 
      let size = get_size typ in
      Smap.add name (-next) env, next + size)
      (Smap.empty,0) 
      (List.map (fun x -> (fst x, (snd x).vdecl_typ)) e.decl.fdecl_args)
	in
   let () = envf := Smap.add f sizearg !envf in
   let return_size = get_size e.decl.fdecl_rty in
   let Bloc(el),_,_ = alloc_bloc env 0 e.body in
   Fun(return_size,f, el, !fpmax)

let alloc = List.map alloc_function

(* compilation phase *)

let popn n = addq (imm n) (reg rsp)
let pushn n = subq (imm n) (reg rsp)

let movnto n =
	let code = ref nop in
	for i = 0 to n - 1 do
		code := !code ++
		movq (ind ~ofs:((n-(i+1))*8) rsp) (reg r11) ++
		movq (reg r11) (ind ~ofs:(i*8) rcx)
	done;
	!code ++
	popn (n * 8)

let movnfrom n =
	let code = ref nop in
	for i = 0 to n - 1 do
		code := !code ++

		movq (ind ~ofs:(i*8) rcx) (reg r11) ++
		pushq (reg r11)
	done;
	!code

let while_num = ref 0
let if_num = ref 0
let counter = ref 0 


let rec compile_unop e env1 env2 size fpmax = function
	| Ast.Uneg -> negq (reg rax)++
			  pushq (reg rax)
	| Ast.Unot -> notq (reg rax) ++
			  pushq (reg rax)
	| Ast.Uderef -> 
		begin let code =ref nop in
		for i = 0 to (size/8-1) do 
			code:= !code ++
				pushq(ind ~ofs: (i*8) rax)		
		done;
		!code
		end
	| _ ->  
		begin match e with 
			| Unop(Ast.Uderef,e2,_) -> compile_expr env1 env2 fpmax e2
			| Lvar(ofs_x) -> 
				movq (reg rbp) (reg rax) ++ 
			   addq (imm ofs_x) (reg rax) ++ 
				pushq (reg rax) 
			| Gvect (e1,e2,size) -> 
			   compile_expr env1 env2 fpmax e1 ++ 
			   popq rax ++ 
			   compile_expr env1 env2 fpmax e2 ++ 
			   popq rbx ++
			   popq rax ++ 
       		   imulq (imm (size/8)) (reg rbx) ++
			   addq (reg rbx) (reg rax) ++
			   pushq (reg rax) 
			| Gstruct _ -> nop	 
			| _ -> assert(false) 
				(* ce cas ne doit pas arriver *)
		end  


and  compile_binop b env1 env2 fpmax e2 = 
   let f = string_of_int in
	let open Ast in
	counter:=1+ !counter; 
	match b with
	| Badd -> compile_expr env1 env2 fpmax e2 ++
			  popq rax ++
			  addq (reg rbx) (reg rax)
	| Bsub -> compile_expr env1 env2 fpmax e2 ++
			  popq rax ++
			  subq (reg rbx) (reg rax)
	| Bmul -> compile_expr env1 env2 fpmax e2 ++
			  popq rax ++
			  imulq (reg rbx) (reg rax)
	| Band -> testq (reg rbx) (reg rbx) ++
			  jz ("fin_" ^ (f !counter)) ++
			  compile_expr env1 env2 fpmax e2 ++
			  popq rax ++
			  andq (reg rbx) (reg rax) ++
			  jmp ("final_" ^ (f !counter)) ++
			  label ("fin_" ^ (f !counter)) ++
			  xorq (reg rax) (reg rax)++
			  label ("final_" ^ (f !counter))
  	| Bor ->  testq (reg rbx) (reg rbx) ++
			  jnz ("fin_" ^ (f !counter)) ++
			  compile_expr env1 env2 fpmax e2 ++
			  popq rax ++
  			  orq (reg rbx) (reg rax) ++
			  jmp ("final_" ^ (f !counter)) ++
			  label ("fin_" ^ (f !counter)) ++
			  movq (reg rbx) (reg rax) ++
			  label ("final_" ^ (f !counter))
	| Bdiv | Bmod as l-> 
			cqto ++
			idivq (reg rbx) ++
			if l = Bmod then		 	
	 		movq (reg rdx) (reg rax)
	 		else nop
	| Baff -> assert false	 	
	| _ as l -> 
         begin
			let f = match l with 
			| Beq -> cmove (reg r11) (reg r13)
			| Bneq -> cmovne (reg r11) (reg r13) 
			| Blt -> cmovl (reg r11) (reg rax)
			| Ble -> cmovle (reg r11) (reg rax)
			| Bgt -> cmovg (reg r11) (reg rax)
			| Bge -> cmovge (reg r11) (reg rax)
			in
			xorq (reg r13) (reg r13) ++
         movq (imm 1) (reg r11) ++
			cmpq (reg rbx) (reg rax) ++
 			f ++
		 	movq (reg r13) (reg rax)
         end

and compile_baff envfunction envstruct fpmax e1 e2 size =
	let code = ref nop in
	for i=0 to size/8-1 do
		code:= !code++
		popq rdx ++
		movq (reg rdx) (ind ~ofs:(size - i * 8) rcx) 
	done;
	compile_expr envfunction envstruct fpmax (Unop(Uborr, e1, size)) ++
	popq rcx ++
	compile_expr envfunction envstruct fpmax e2 ++
	!code

and compile_expr envfunction envstruct fpmax = function
	| None -> nop
	| Cst c -> begin match c with 
				| Cint i ->  pushq (imm i)
				| Cbool b -> if b then pushq (imm 1)
							 else pushq (imm 0) 
				end
	| Lvar fp_x -> pushq (ind ~ofs:fp_x rbp)
	| Binop (op,e1,e2,size) when op = Baff -> 
		compile_baff envfunction envstruct fpmax e1 e2 size
	| Binop (op,e1,e2,size) -> 
		compile_expr envfunction envstruct fpmax e1 ++
		popq rbx ++
		compile_binop op envfunction envstruct fpmax  e2
	| Unop (o,e1,size) ->
		compile_expr envfunction envstruct fpmax e1 ++
		popq rax ++
		compile_unop e1 envfunction envstruct size fpmax o
	| Len (e) ->
      compile_expr envfunction envstruct fpmax e ++
      popq r11
	| Print (s) -> let () = strings := s :: !strings in
				   let ind = string_of_int !string_ind in
				   let () = incr string_ind in
				   movq (ilab ("string" ^ ind)) (reg rdi) ++
				   call "print_string"
	| Call (argsize,f,el,pushsize) ->
		let code1 =
		List.fold_left 
      (fun code e -> code ++ compile_expr envfunction envstruct fpmax e) 
      nop el ++
     	call f ++
     	popn argsize 
     	(* le -8 c'est la valeur de ret; le ret prend en charge ceci *)
     	in
     	let code2 = ref nop in
		for i=0 to (pushsize/8 -1) do
			code2 := !code2 ++
			movq (ind ~ofs:(pushsize-((i+1)*8)) r13) (reg r12) ++
			pushq (reg r12)
			(* lecture du haut en bas pour éviter 
			d'ecraser des donnees utiles *)
		done;
		code1 ++ !code2
     	 
	| Bloc b -> compile_bloc envfunction envstruct fpmax b
	| While (e,b) -> 
		while_num:=1 + !while_num;
		(* petite optimisation *)
		let s = string_of_int !while_num in
		jmp ("loop_test_" ^ s) ++
		label ("loop_body_" ^ s) ++
		compile_bloc envfunction envstruct fpmax b ++
		label ("loop_test_" ^ s) ++
		compile_expr envfunction envstruct fpmax e ++
		popq rax ++
		testq (reg rax) (reg rax) ++
		jnz ("loop_body_" ^ s)

	| If (e1,e2,e3) -> 
      let () = incr if_num in
		let end_if = string_of_int !if_num in
      let Bloc b1 = e2 in
      let Bloc b2 = e3 in
		compile_expr envfunction envstruct fpmax e1 ++
		popq rax ++
		testq (reg rax) (reg rax) ++
		jz ("end_if" ^ end_if) ++ 
		compile_bloc envfunction envstruct fpmax b1 ++
		jmp ("end_else" ^ end_if) ++
		label ("end_if" ^ end_if) ++
		compile_bloc envfunction envstruct fpmax b2 ++
		label ("end_else" ^ end_if)

	| Ret (e,i) -> 
		begin match e with
			| None -> 	
				movq (reg rsp) (reg r13) ++
            movq (reg rbp) (reg rsp) ++
            popq rbp ++
				ret
			| _ -> 
				compile_expr envfunction envstruct fpmax e ++ 
				movq (reg rsp) (reg r13) ++
            movq (reg rbp) (reg rsp) ++
            popq rbp ++
				ret

		end
	| Lets (fp_x,el) -> 
		let mycode=ref nop in
		let a,b =List.fold_left 
			(fun (code,counter) (id,e) -> 
				for i=counter to id/8-1 do
					mycode := !mycode ++
					popq rax ++
					movq (reg rax) (ind ~ofs:(fp_x+i) rbp)
				done;
				let counter = id/8-1 in
				compile_expr envfunction envstruct fpmax e ++
				!mycode,counter) (nop,0) el 
		in
		a
	| Gstruct (e,sizeret,pos) -> 
		let code = ref nop in
		for i=0 to sizeret/8-1 do 
			code:=!code ++
			movq (ind ~ofs: (pos+(sizeret/8-1-i)) rbp) (reg rax)++
			pushq (reg rax)
		done;
		movq (reg rsp) (reg r11) ++
		compile_expr envfunction envstruct fpmax e ++
		movq (reg r11) (reg rsp) ++
		!code
	| Gvect (e1,e2,size) ->
		compile_expr envfunction envstruct fpmax e1 ++
		popq rcx ++
		popq rbx ++
		compile_expr envfunction envstruct fpmax e2 ++
		popq rax ++
		addq (reg rax) (reg rcx) ++
		movnfrom (size/8)
	| Vect (el,i2) ->
		let l = List.length el in
		let size = l * i2 in
		let code,_ =
			List.fold_left
			(fun (code, index) e ->
				(
				code ++
				compile_expr envfunction envstruct fpmax e ++
				movq (reg rdi) (reg rcx) ++
				addq (imm (index * i2)) (reg rcx) ++
				movnto (i2/8)
				,
				index + 1
				)
			)
			(nop, 0)
			el
		in
		movq (imm size) (reg rax) ++
		call "malloc" ++
		pushq (imm l) ++
		code ++
		pushq (reg rdi)
	| Letv (m,e,size) ->
		compile_expr envfunction envstruct fpmax e ++
		movq (imm m) (reg rcx) ++
      addq (reg rbp) (reg rcx) ++
		movnto (size/8)

and compile_bloc envfunction envstruct fpmax = function 
	| [] -> nop
	| r::s -> compile_expr envfunction envstruct fpmax r ++
			  compile_bloc envfunction envstruct fpmax s 

let rec compile_decl_function e =
	match e with 
	| Fun (sizeret,s,el,fpmax) ->
      let code =
         List.fold_left 
         (fun code e -> code ++ (compile_expr !envf !envs fpmax e) ) 
         nop el
      in 
		label s ++
		pushq (reg rbp) ++
		movq (reg rsp) (reg rbp) ++
		pushn fpmax ++
      code ++
		movq (reg rsp) (reg r13) ++
      movq (reg rbp) (reg rsp) ++
		popq rbp ++
		ret
	|_ -> assert (false) 
	(* ce cas ne doit jamais arriver *) 

(* 
and	compile_if (e,b,els)=
	if_num:=1+!if_num ;
	let assembly={ compile_expr e ++
	popq (reg rax) ++
	testq (reg rax) (reg rax) ++
	jz ("end_if"^end_if) ++ 
	compile_bloc envfunction envstruct fpmax b ++	} in
	match els with 
		| None -> 
			assembly ++
			label ("end_if"^end_if)
		| Some (Bloc (b)) ->
			assembly ++
			jmp ("end_else"^end_if) ++
			label ("end_if"^end_if) ++
			compile_bloc envfunction envstruct fpmax b ++
			label ("end_else"^end_if)
		| Some (If (b)) ->
			assembly ++
			jmp ("end_else"^end_if) ++
			label ("end_if"^end_if) ++
			compile_if b ++
			label ("end_else"^end_if)
 pour les anciens if else   *)


let compile_program prog filename =
	(* plein d'instructions*)
	let structenv = Smap.empty in
	let structenv =
		List.iter
		(fun structid ->
			alloc_structure 
				structid 
				(Decls.find structid prog.file_structs) 
		)
		prog.struct_order
	in
	let funl =
		List.fold_left
		(fun l funid ->
         let fun_decl =
            alloc_function 
            funid 
            (Decls.find funid prog.file_funs)
         in
         fun_decl :: l
		)
      []
		prog.fun_order
	in
	let compiled_functions = 
		List.fold_left 
		(fun code e -> code ++ compile_decl_function e)
		nop
		funl
	in
	let l = List.length !strings in
	let strings,_ =
		List.fold_left
		(fun (code,index) s ->
			let indexs = string_of_int index in
			(
			code ++
			label ("string" ^ indexs) ++
			string s
			,
			index - 1
			)
		)
		(nop, l - 1)
		!strings
	in 
	let p={
	text = globl "main" ++
	compiled_functions ++
	xorq (reg rax) (reg rax) ++
	ret ++
	label "print_string" ++
	xorq (reg rax) (reg rax) ++
	call "printf" ++
	ret;
	data = (* peut etre d'autres instructions *)
		strings
	}
	in print_in_file filename p

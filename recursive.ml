module Term = 


		struct

		type ty =
			| TyNat
			| TyBool
			| TyRecord of (string * ty) list
			| TyTop
			| TyArr of ty * ty
			| TyUnit
			| TyListNat
			

		type binding =
			| NameBind
			| VarBind of ty
			
			
		(*type listnat = 
			| Nil
			| Cons of term * listnat*)
			
			
	   	type term =
			| TmTrue
			| TmFalse			
			| TmZero
			| TmPred of term
			| TmSucc of term
			| TmIsZero of term 
			| TmIf of term * term * term
			| TmRecord of (string * term) list
			| TmProj of  term * string
			| TmVar of int * int
			| TmAbs of string * ty * term
			| TmApp of term * term
			| TmListNat of term
			| TmNil
			| TmCons of term * term
			
			
		exception Err of string
		
		type context = (string * binding) list		(* context represented as a list of strings and associated bindings *)
	
	
		(* Returns the length of the context *)
		let ctxlength ctx =
			List.length ctx
			
		let getbinding ctx i = (*The getbinding function simply looks up the ith binding in the given context*)
			try
				let (_, bind) = List.nth ctx i in
				bind
			with Failure _ -> raise (Err "[not found]")



		let rec index2name ctx x =
			try
				let (name, _) = List.nth ctx x in
				name
			with Failure _ -> "[not found]"



		(* Returns true if a name is already in the context, false otw *)
		let rec alreadyused ctx x =
			match ctx with
			| [] -> false
			| (name, _) :: l1 ->
				if name = x then true
				else alreadyused l1 x




		(* adds a name to the context, creating new name if already used *)
		let rec pickfreshname ctx x bind =
			if alreadyused ctx x 
			then pickfreshname ctx (x ^ "'") bind				(* add an apex to the name if it was already used *)
			else ((x, bind)::ctx), x				   	 (* if the name is not already used, add it to ctx *)






		let getTypeFromContext ctx i =			(*extract the typing assumption associated with a particular variable i in a context ctx*)
			match getbinding ctx i with
				| VarBind(tyT) -> tyT
				(*| _ -> error fi ("getTypeFromContext: Wrong kind of binding for variable "^ (index2name fi ctx i))*)
				| _ -> let str = "getTypeFromContext: Wrong kind of binding for variable " ^ (index2name ctx i) in		
						raise (Err str)
						
		(* Checks whether the term as argument is a numeric value or not *)
		let rec isnumericval t =								
			match t with 
			| TmZero -> true 
			| TmSucc(t1) -> isnumericval t1 
			| _ -> false


		(* Checks whether the term as argument is a value (boolean or numeric value) or not *)	
		let rec isval t = 
			match t with 
			| TmTrue -> true
			| TmFalse -> true
			| TmAbs(_, _, _) -> true
			| t when isnumericval t -> true 
			| _ -> false
			
			
		let rec to_string ctx t =  (*String conversion*)
			match t with 

			| TmTrue -> "True"
			| TmFalse -> "False"
			| TmZero ->  "0"
			| TmPred(t) -> let str = to_string ctx t in "pred(" ^ (str) ^ ")"
			| TmSucc(t) -> let str = to_string ctx t in "succ("^ (str) ^ ")"
			| TmIsZero(t)  ->  let str = to_string ctx t in "isZero(" ^ (str) ^ ")"
			| TmIf(t1,t2,t3) -> let str1 = to_string ctx t1 in 
							let str2 = to_string ctx t2 in  
							let str3 = to_string ctx t3 in 
							"If (" ^ str1 ^ ") then (" ^ str2 ^ ") else (" ^ str3 ^ ")"
			| TmAbs(x, ty, t1) -> let (ctx',x') = pickfreshname ctx x (VarBind(ty)) in					(* A new name is chosen for x if it already exists *)
							"(λ " ^ x' ^ ". " ^ (to_string ctx' t1) ^ ")"
			| TmApp(t1, t2) -> "(" ^ to_string ctx t1 ^ " " ^ to_string ctx t2 ^ ")"
			| TmVar(x,n) -> if ctxlength ctx = n then									(* Consistency check. If it doesn't pass a shift is missing somewhere *)
							(index2name ctx x)					     				(* return the name corresponding to the index in ctx *)
							else
							let ctxl = ctxlength ctx in 
							"[bad index, n is " ^ (string_of_int n) ^ " and length is " ^ (string_of_int ctxl) ^ "]"
			| _ -> raise (Err "No match")


		let rec to_string_nofresh ctx t =  (*String conversion, doesn't pick a fresh name*)
			match t with 

			| TmTrue -> "True"
			| TmFalse -> "False"
			| TmZero ->  "0"
			| TmPred(t) -> let str = to_string ctx t in "pred(" ^ (str) ^ ")"
			| TmSucc(t) -> let str = to_string ctx t in "succ("^ (str) ^ ")"
			| TmIsZero(t)  ->  let str = to_string ctx t in "isZero(" ^ (str) ^ ")"
			| TmIf(t1,t2,t3) -> let str1 = to_string ctx t1 in 
							let str2 = to_string ctx t2 in  
							let str3 = to_string ctx t3 in 
							"If (" ^ str1 ^ ") then (" ^ str2 ^ ") else (" ^ str3 ^ ")"
			| TmAbs(x, ty, t1) -> (*let (ctx',x') = pickfreshname ctx x (VarBind(ty)) in	*)				(* A new name is chosen for x if it already exists *)
							"(λ " ^ x ^ ". " ^ (to_string ctx t1) ^ ")"
			| TmApp(t1, t2) -> "(" ^ to_string ctx t1 ^ " " ^ to_string ctx t2 ^ ")"
			| TmVar(x,n) -> if ctxlength ctx = n then									(* Consistency check. If it doesn't pass a shift is missing somewhere *)
							(index2name ctx x)					     				(* return the name corresponding to the index in ctx *)
							else
							let ctxl = ctxlength ctx in 
							"[bad index, n is " ^ (string_of_int n) ^ " and length is " ^ (string_of_int ctxl) ^ "]"
			| _ -> raise (Err "No match")





		let rec printtm ctx t =  (*term print*)
			match t with 

			| TmTrue -> Format.printf"True"
			| TmFalse ->  Format.printf "False"
			| TmZero ->  Format.printf "0";
			| TmPred(t) -> Format.printf "pred("; let str = to_string ctx t in Format.printf ("%s") (str); Format.printf ")"
			| TmSucc(t) -> Format.printf "succ(";Format.printf ("%s") (to_string ctx t) ;Format.printf")"
			| TmIsZero(t)  ->   Format.printf "isZero(" ;Format.printf ("%s") (to_string ctx t) ;Format.printf")"
			| TmIf(t1,t2,t3) -> Format.printf "If ("; Format.printf ("%s") (to_string ctx t1) ; Format.printf ") then ("; Format.printf ("%s") (to_string ctx t2); Format.printf ") else ("; Format.printf ("%s") (to_string ctx t3); Format.printf ")"
			| TmAbs(x, ty, t1) -> let (ctx',x') = pickfreshname ctx x (VarBind(ty)) in						(* A new name is chosen for x if it already exists *)
						Format.printf "(λ "; Format.printf "%s. " x'; Format.printf "%s)" (to_string ctx' t1)
			| TmApp(t1, t2) -> Format.printf ("(%s ") (to_string ctx t1); Format.printf ("%s )") (to_string ctx t2)
			| TmVar(x,n) -> if ctxlength ctx = n then									(* Consistency check. If it doesn't pass a shift is missing somewhere *)
							Format.printf "%s" (index2name ctx x)					     				(* return the name corresponding to the index in ctx *)
							else
							let ctxl = ctxlength ctx in 
							Format.printf ("[bad index, n is %s") (string_of_int n); Format.printf (" and length is %s]") (string_of_int ctxl)
			| _ -> raise (Err "No match")




		let hd l = 
			match l with 
				| TmListNat(t) -> (match t with
									| TmNil -> TmZero
									| TmCons(h, t) -> h
									| _ -> raise (Err "List not valid")
									)
				| _ -> raise (Err "hd function can be called on lists only")
			
		let tl l = 
			match l with 
				| TmListNat(t) -> (match t with
									| TmNil -> TmZero
									| TmCons(h, t1) -> TmListNat(t1)
									| _ -> raise (Err "List not valid")
									)
				| _ -> raise (Err "tl function can be called on lists only")
			
			
		let isnil t =
			match t with 
				| TmListNat(t) -> (match t with
									| TmNil -> true
									| TmCons(h, t) -> false
									| _ -> raise (Err "List not valid")
									)
				| _ -> raise (Err "isnil function can be called on lists only")
			
			
		(* s:NatList→Nat. λl:NatList. *)
		let rec sumlist s l = 
			let rec sl s l =
				if isnil l
				then (match s with | TmListNat(t) -> t | _ -> TmNil)
				else TmCons((hd l), (sl s (tl l)))
			in TmListNat(sl s l)
			
			
			
		let rec llength l =
			let rec lllength l =
				match l with
					| TmNil -> 0
					| TmCons(h, t) -> 1 + (lllength t)
					| _ -> raise (Err "List not valid")
					
			in match l with 
				| TmListNat(t) -> lllength t
				| _ -> raise (Err "llength function can be called on lists only")
			
		
		
		(*Type checker*)
		let rec typeof ctx t =
			match t with
			| TmTrue -> TyBool

			| TmFalse -> TyBool

			| TmZero -> TyNat

			| TmPred(t) ->  if (=) (typeof ctx t) TyNat then TyNat
										else TyUnit(*raise (Err "t hasn't a predecessor")*)

			| TmSucc(t) -> if (=) (typeof ctx t) TyNat then TyNat
										else TyUnit

			| TmIsZero(t) -> if (=) (typeof ctx t) TyNat then TyBool
										else TyUnit

			| TmIf(t1,t2,t3) ->  if (=) (typeof ctx t1) TyBool then
										let tyT2 = typeof ctx t2 in
										if (=) tyT2 (typeof ctx t3) then tyT2
										else TyUnit
									else TyUnit

			| TmVar(i,_) -> getTypeFromContext ctx i

			| TmAbs(x,tyT1,t2) -> 	let (ctx', x') = pickfreshname ctx x (VarBind(tyT1)) in
									let tyT2 = typeof ctx' t2 in
									TyArr(tyT1, tyT2)

			| TmApp(t1,t2) -> 	let tyT1 = typeof ctx t1 in
									let tyT2 = typeof ctx t2 in
									(match tyT1 with
										| TyArr(tyT11,tyT12) -> if (=) tyT2 tyT11 then tyT12
																else TyUnit
										| _ -> TyUnit)
										
			 | TmRecord(fields) ->
                                   let fieldtys = 
                                   List.map (fun (li,ti) -> (li, typeof ctx ti)) fields in
                                   TyRecord(fieldtys)
									
			| TmListNat(ln) -> (match ln with
								| TmNil -> TyListNat
								| TmCons(n, l1) when isnumericval n -> typeof ctx l1
								| _ -> TyUnit
								)

			| TmNil -> TyListNat

			| TmCons(n, l1) when isnumericval n -> typeof ctx l1
			
			| _ -> raise (Err "No match")

						
						
						
						
						
						
								
		let rec subtype tyS tyT = 
			(=) tyS tyT || 																					(*S-REFL*)
			(match (tyS,tyT) with
				| (TyRecord(fS), TyRecord(fT)) ->  List.for_all
							(fun (li,tyTi) ->
								try let tySi = List.assoc li fS in
									subtype tySi tyTi  														(*S-RCDDEPTH*)
								with Not_found -> false)
							fT
				| (_,TyTop)  -> true																		(*S-TOP*)
				| (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->  (subtype tyT1 tyS1) && (subtype tyS2 tyT2) 		(*S-ARROW*)
				| (_,_) -> false
			)
			
			
		let rec type_to_string ty =
			match ty with
			| TyNat -> "Nat"
			| TyBool -> "Bool"
			| TyArr(ty1, ty2) -> let ty11 = type_to_string ty1 in
								let ty22 = type_to_string ty2 in
								ty11 ^ " --> " ^ ty22
		
		
								
		
		
		let rec printed_typeof ctx t =
			match t with
			| TmTrue -> Format.printf "True: Bool \n"
			
			| TmFalse -> Format.printf "False: Bool \n"
			
			| TmZero -> Format.printf "0: Nat \n"
			
			| TmPred(t) ->  if (=) (typeof ctx t) TyNat then (let str = to_string ctx t in Format.printf "Pred %s: Nat" (str); Format.printf "\n "; printed_typeof ctx t;)
												else Format.printf "t hasn't a predecessor"
			
			| TmSucc(t) -> if (=) (typeof ctx t) TyNat then (Format.printf "Succ %s: Nat" (to_string ctx t); Format.printf "\n "; printed_typeof ctx t )
										else Format.printf "t hasn't a successor"
										
			| TmIsZero(t) -> if (=) (typeof ctx t) TyNat then (Format.printf "IsZero %s: Bool" (to_string ctx t); Format.printf "\n "; printed_typeof ctx t)
										else Format.printf "t hasn't a successor"
			
			(* | TmIf(t1,t2,t3) ->  if (=) (typeof ctx t1) TyBool then
														(let tyT2 = typeof ctx t2 in
														if (=) tyT2 (typeof ctx t3) then (Format.printf "If ("; (printed_typeof ctx t1); Format.printf ") then ("; (printed_typeof ctx t2); Format.printf ") else ("; (printed_typeof ctx t3); Format.printf ") \n")
														else (Format.printf "arms of conditional have different types"))
													else Format.printf "guard of conditional not a boolean" *)
													
			| TmIf(t1,t2,t3) ->  if (=) (typeof ctx t1) TyBool 
							then
								(let tyT2 = typeof ctx t2 in
								if (=) tyT2 (typeof ctx t3) 
								then (printtm ctx t; Format.printf "\n T1: "; (printed_typeof ctx t1); Format.printf "\n T2: "; (printed_typeof ctx t2); Format.printf "\n T3: "; (printed_typeof ctx t3); Format.printf " \n\n")
								else (Format.printf "arms of conditional have different types"))
							else Format.printf "guard of conditional not a boolean"
									
			| TmVar(i,_) -> let str = to_string ctx t in
							let typctx = getTypeFromContext ctx i in
							let typ = type_to_string typctx in
							Format.printf "VAR %s: %s \n" (str) (typ)
			
			| TmAbs(x,tyT1,t2) -> 	let (ctx', x') = pickfreshname ctx x (VarBind(tyT1)) in
									let tyT2 = typeof ctx' t2 in printed_typeof ctx' t2;
									let str = type_to_string (TyArr(tyT1, tyT2)) in
									let lam = to_string_nofresh ctx' t in
									Format.printf "ABS %s: %s \n" (lam) (str)
										
			| TmApp(t1,t2) -> 	let tyT1 = typeof ctx t1 in
								let tyT2 = typeof ctx t2 in
								(match tyT1 with
									| TyArr(tyT11,tyT12) -> if (=) tyT2 tyT11 
															then (printed_typeof ctx t1; printed_typeof ctx t2; let typ = type_to_string tyT12 in Format.printf "APP %s \n" typ)
															else Format.printf "parameter type mismatch"
									| _ -> Format.printf "arrow type expected"
								)
						
							
		
				
				
				
		(* Defines shifting operation *)
		(* 6.2.1 pag 79 *)	
		let termShift d t =
			let rec walk c t = 
				match t with
				| TmVar(x, n) -> if x >= c 							(* The index has to be shifted only if it is greater than the cutoff *)
								then TmVar(x + d, n + d)
								else TmVar(x, n + d)
				| TmAbs(x, ty, t1) -> TmAbs(x, ty, walk (c + 1) t1)			(* Every time a bound is encountered, the cutoff increases by one and walk is called on the subterm *)
				| TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)		(* In applications the shifting is applied on both subterms *)
				| _ -> t
			in walk 0 t							(* the first call is walk on the original term with cutoff = 0 *)


		(* Defines substitution operation. *)
		(* 6.2.4 pag 80 *)
		let termSubst j s t =  (* [j -> s]t *)
			let rec walk c t = 
				match t with
				| TmVar(x, n) -> if x = j + c   					(* if x is exactly j plus the cutoff *)
								then termShift c s      			(* all the j in t are substituted with s *)
								else TmVar(x, n)					(* nothing happens *)
				| TmAbs(x, ty, t1) -> TmAbs(x, ty, walk (c + 1) t1)			(* call the walk function increasing the cutoff *)
				| TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)		(* call the walk on the two subterms *)
				| _ -> t
			in walk 0 t


		let termSubstTop s t =
			termShift (-1) (termSubst 0 (termShift 1 s) t)				(* the term being substituted for the bound variable is first shifted up by one, 
																then the substitution is made, and then the resulting term is shifted down because
																a bound variable has been used *)


		(* Single step evaluation *)
		let rec eval1 ctx t = 
			match t with
			| TmIf(TmTrue , t2, t3) -> t2                                             				(*E_iftrue*)
			| TmIf(TmFalse, t2, t3) -> t3 															(*E_iffalse*)	
			| TmIf(t1, t2, t3) -> let t1' = eval1 ctx t1 in TmIf(t1', t2, t3)							(*E_if*)
			| TmSucc(t1) -> let t1' = eval1 ctx t1 in TmSucc (t1')                      				(*E_succ*)
			| TmPred(TmZero) -> TmZero													(*E_predzero*)
			| TmPred(TmSucc(nv1)) when (isnumericval nv1) -> nv1								(*E_predsucc*)
			| TmPred(t1) -> let t1' = eval1 ctx t1 in TmPred(t1')									(*E_pred*)
			| TmIsZero (TmZero) -> TmTrue 												(*E_iszerozero*)
			| TmIsZero (TmSucc(nv1)) when (isnumericval nv1) -> TmFalse					 		(*E_iszerosucc*)
			| TmIsZero (t1) -> let t1' = eval1 ctx t1 in TmIsZero(t1')								(*E_iszero*)	
			| TmApp(TmAbs(x, ty, t12), v2) when isval v2 && subtype (typeof ctx v2) (ty) -> termSubstTop v2 t12    			(*E_APPABS*)
			| TmApp(v1, t2) when isval v1 -> let t2' = eval1 ctx t2 in TmApp(v1, t2')     			(*E_APP2*)
			| TmApp(t1, t2) -> let t1' = eval1 ctx t1 in TmApp(t1', t2)                                 	(*E_APP1*)
			| TmRecord(fields) -> let rec evalafield l = (match l with 
                                                          [] -> raise (Err "")
                                                          | (l,vi)::rest when isval vi -> 
                                                          let rest' = evalafield rest in
                                                          (l,vi)::rest'
                                                          | (l,ti)::rest -> 
                                                          let ti' = eval1 ctx ti in
                                                          (l, ti')::rest)
                                                          in let fields' = evalafield fields in
                                                          TmRecord(fields')
			| _ -> raise (Err "")												



		(* Same behaviour as eval1 but prints the rule applied *)
		let rec printEvalStep ctx t = 
			(match t with 
			| TmIf (TmTrue,t2,t3) -> Format.printf " E_iftrue \n"; t2                                   
			| TmIf (TmFalse,t2,t3) -> Format.printf " E_iffalse \n"; t3						
			| TmIf (t1,t2,t3) -> Format.printf " E_if \n"; let t1' = printEvalStep ctx t1 in TmIf(t1', t2, t3)					
			| TmSucc (t1) ->  Format.printf " E_succ \n"; let t1' = printEvalStep ctx t1 in TmSucc (t1')                       							
			| TmPred (TmZero) -> Format.printf " E_predzero \n "; TmZero 								    
			| TmPred (TmSucc(nv1)) when (isnumericval nv1) ->	Format.printf " E_predsucc \n"; nv1		
			| TmPred (t1) -> Format.printf " E_pred \n"; let t1' = printEvalStep ctx t1 in TmPred(t1')							                    
			| TmIsZero (TmZero) -> Format.printf " E_iszerozero \n"; TmTrue		
			| TmIsZero (TmSucc(nv1)) when (isnumericval nv1) -> Format.printf " E_iszerosucc \n"; TmFalse  	
			| TmIsZero (t1) -> Format.printf " E_iszero \n"; let t1' = printEvalStep ctx t1 in TmIsZero(t1')	
			| TmApp(TmAbs(x, ty, t12), v2) when isval v2 && subtype (typeof ctx v2) (ty) -> Format.printf "E_APPABS \n"; termSubstTop v2 t12            			 (*E_APPABS*)
			| TmApp(v1, t2) when isval v1 -> Format.printf "E_APP2 \n"; let t2' = printEvalStep ctx t2 in TmApp(v1, t2')     	 (*E_APP2*)
			| TmApp(t1, t2) -> Format.printf "E_APP1 \n"; let t1' = printEvalStep ctx t1 in TmApp(t1', t2)                          (*E_APP1*)
			| _ -> raise (Err "")		
			 )  


		(* Multi-step evaluation *)
		let rec eval ctx t = 
			try let t' = eval1 ctx t in eval ctx t'
			with Err "" -> t

		(* Printed multi-step evaluation *)
		let rec printedEval ctx t =
			try let t' = printEvalStep ctx t in printedEval ctx t'			(* printedEval doesn't print when eval1 calls itself. Can do better*)
			with Err "" -> t


		(* Big-step evaluation *)
		let rec bigstep ctx t =
			match t with 
			| TmFalse | TmTrue | TmZero -> t	           					 			(*B_Value*)
			| TmIf(t1,t2,t3) -> let t1' = bigstep ctx t1 in       
						(match t1' with
						| TmTrue -> let v2 = bigstep ctx t2 in   		 			 		(*B_IfTrue*)
						v2
						| TmFalse -> let v3 = bigstep ctx t3 in	 				     	(*B_IfFalse*)
						v3
						| _ -> raise (Err "Invalid argument. TmIf accepts only TmTrue and TmFalse as argument.")
						)
			| TmSucc(t1) -> let nv1 = bigstep ctx t1 in								 		(*B_Succ*)
						if isnumericval nv1 
						then TmSucc(nv1)
						else raise (Err "Invalid argument. TmSucc accepts only numeric values as argument.")
			| TmPred(t1) -> let t1' = bigstep ctx t1 in 
						(match t1' with 
						| TmZero -> t1'									         	(*B_PredZero*)
						| TmSucc(nv1) when (isnumericval nv1) -> nv1		     			(*B_PredSucc*)
						| _ -> raise (Err "Invalid argument. TmPred accepts only numeric values as argument.")
						)
			| TmIsZero(t1) -> let t1' = bigstep ctx t1 in 
						(match t1' with 
						| TmZero -> TmTrue									     	(*B_IsZeroZero*)
						| TmSucc(nv1) when (isnumericval nv1) -> TmFalse		 			(*B_IsZeroSucc*)
						| _ -> raise (Err "Invalid argument. TmZero accepts only numeric values as argument.")
						)
			| TmAbs(x, ty, t) -> TmAbs(x, ty, t)
			| TmApp(t1, t2) -> let t1' = bigstep ctx t1 in
							(match t1' with
							| TmAbs(x, ty, t12) -> let v2 = bigstep ctx t2 in
											let t' = bigstep ctx (termSubstTop v2 t12) in
											t'
							| _ -> raise (Err "")

							)



		let rec printedBigstep ctx t =
			match t with 
			| TmFalse | TmTrue | TmZero -> Format.printf " B_Value \n"; t	           						(*B_Value*)
			| TmIf(t1,t2,t3) -> let t1' = printedBigstep ctx t1 in       
						(match t1' with
						| TmTrue -> Format.printf " B_IfTrue \n"; let v2 = printedBigstep ctx t2 in 				(*B_IfTrue*)
						v2
						| TmFalse -> Format.printf " B_IfFalse \n"; let v3 = printedBigstep ctx t3 in	 			(*B_IfFalse*)
						v3
						| _ -> raise (Err "Invalid argument. TmIf accepts only TmTrue and TmFalse as argument.")
						)
			| TmSucc(t1) -> let nv1 = printedBigstep ctx t1 in												(*B_Succ*)
						if isnumericval nv1 
						then (Format.printf " B_Succ \n"; TmSucc(nv1))
						else raise (Err "Invalid argument. TmSucc accepts only numeric values as argument.")
			| TmPred(t1) -> let t1' = printedBigstep ctx t1 in 
						(match t1' with 
						| TmZero -> Format.printf " B_PredZero \n"; t1'									(*B_PredZero*)
						| TmSucc(nv1) when (isnumericval nv1) -> Format.printf " B_PredSucc \n"; nv1		     (*B_PredSucc*)
						| _ -> raise (Err "Invalid argument. TmPred accepts only numeric values as argument.")
						)
			| TmIsZero(t1) -> let t1' = printedBigstep ctx t1 in 
						(match t1' with 
						| TmZero -> Format.printf " B_IsZeroZero \n"; TmTrue								(*B_IsZeroZero*)
						| TmSucc(nv1) when (isnumericval nv1) -> Format.printf " B_IsZeroSucc \n"; TmFalse		(*B_IsZeroSucc*)
						| _ -> raise (Err "Invalid argument. TmZero accepts only numeric values as argument.")
						)
			| TmAbs(x, ty, t) -> Format.printf "B-ABS\n"; TmAbs(x, ty, t)
			| TmApp(t1, t2) -> let t1' = printedBigstep ctx t1 in
							(match t1' with
							| TmAbs(x, ty, t12) -> Format.printf "B-APPABS\n"; let v2 = printedBigstep ctx t2 in
											let t' = printedBigstep ctx (termSubstTop v2 t12) in
											t'
							| _ -> raise (Err "")

							)







			let multi_vs_Bigstep ctx t = 
			Format.printf "----------------------------------------------\n";
			Format.printf "THE INSERTED TERM IS:\n%s\n" (to_string ctx t); 
			Format.printf "---------------------------------------------\n\n";
			Format.printf "*** THE BIG STEP EVALUATION IS: \n\n";
			let tBig = printedBigstep ctx t in
			Format.printf "\nThe result of the Big-step evaluation is: %s\n\n" (to_string ctx tBig);
			Format.printf "*** THE MULTI STEP EVALUATION IS: \n\n";
			let tMulti = printedEval ctx t in 
			Format.printf "\nThe result of the Multi-step evaluation is: %s\n\n" (to_string ctx tMulti)




			
		
			
		
			
		
		
		
		
		
		
	end;;
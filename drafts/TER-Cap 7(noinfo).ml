(*An ML Implementation of the Lambda-Calculus*)


module Term = 

	struct
	
		type info = int
		
		type term =
			| TmVar of int * int       		(* int1 : de Brujin index     int2: actual size of the context *)
			| TmAbs of string * term			(* string: name of the variable in the ordinary representation *)
			| TmApp of term * term			
			
			
		type binding = NameBind
		
		type context = (string * binding) list		(* context represented as a list of strings and associated bindings *)
		
		exception NoRuleApplies

	(* Function that returns the nameless representation of the term *)		
	(*let rec removenames ctx t =
		match t with 
		| TmVar(x, ctx.length) -> (* prendo indice dell'ultima occorrenza di x in ctx *)
		| TmAbs(str, ter) -> TmAbs(str, removenames ctx ter)       (* Devo aggiungere str a ctx? *)      
		| TmApp(t1, t2) -> TmApp(removenames ctx t1, removenames ctx t2)
		
	
	(* Function that returns the ordinary representation of the term *)
	let rec restorenames ctx t =
		match t with 
		| TmVar(k, ctx.length) -> (* il k-esimo elemento di ctx *)
		| TmAbs(str, ter) -> TmAbs((*primo nome non in ctx *), restorenames ctx ter)          
		| TmApp(t1, t2) -> TmApp(restorenames ctx t1, restorenames ctx t2)*)
		
	
	
	(* Returns the length of the context *)
	let ctxlength ctx =
		List.length ctx
		
		
		
		
	(* Returns true if a name is already in the context, false otw *)
	let rec alreadyused ctx x =
		match ctx with
		| [] -> false
		| (name, _) :: l1 ->
			if name = x then true
			else alreadyused l1 x



	(* adds a name to the context, creating new name if already used *)
	let rec pickfreshname ctx x =
		if alreadyused ctx x 
		then pickfreshname ctx (x ^ "'") 				(* add an apex to the name if it was already used *)
		else ((x, NameBind)::ctx), x				    (* if the name is not already used, add it to ctx *)
		
		
				
			
	let rec index2name ctx x =
		try
			let (name, _) = List.nth ctx x in
			name
		with Failure _ -> "[not found]"
			

		
	(* Given a context and a term, prints the term giving new names if they already exist in context *)
	let rec printtm ctx t = 
		match t with
		| TmAbs(x,t1) -> let (ctx',x') = pickfreshname ctx x in						(* A new name is chosen for x if it already exists *)
						Format.printf "(λ "; Format.printf "%s" (x'); Format.printf ". "; printtm ctx' t1; Format.printf ")"
		| TmApp(t1, t2) -> Format.printf "("; printtm ctx t1; Format.printf " "; printtm ctx t2; Format.printf ")"
		| TmVar(x,n) -> if ctxlength ctx = n then									(* Consistency check. If it doesn't pass a shift is missing somewhere *)
						Format.printf "%s" (index2name ctx x)						(* return the name corresponding to the index in ctx *)
						else
						let ctxl = ctxlength ctx in 
						Format.printf "[bad index, n is %d and length is %d]" (n) (ctxl)
						
						
	let rec to_string ctx t = 
		match t with
		| TmAbs(x,t1) -> let (ctx',x') = pickfreshname ctx x in						(* A new name is chosen for x if it already exists *)
					"(λ " ^ x' ^ ". " ^ (to_string ctx' t1) ^ ")"
		| TmApp(t1, t2) -> "(" ^ to_string ctx t1 ^ " " ^ to_string ctx t2 ^ ")"
		| TmVar(x,n) -> if ctxlength ctx = n then									(* Consistency check. If it doesn't pass a shift is missing somewhere *)
						(index2name ctx x)					     				(* return the name corresponding to the index in ctx *)
						else
						let ctxl = ctxlength ctx in 
						"[bad index, n is " ^ (string_of_int n) ^ " and length is " ^ (string_of_int ctxl) ^ "]"
						
	
	
	
	(* Defines shifting operation *)
	(* 6.2.1 pag 79 *)	
	let termShift d t =
		let rec walk c t = 
			match t with
			| TmVar(x, n) -> if x >= c 							(* The index has to be shifted only if it is greater than the cutoff *)
							then TmVar(x + d, n + d)
							else TmVar(x, n + d)
			| TmAbs(x, t1) -> TmAbs(x, walk (c + 1) t1)			(* Every time a bound is encountered, the cutoff increases by one and walk is called on the subterm *)
			| TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)		(* In applications the shifting is applied on both subterms *)
							in walk 0 t							(* the first call is walk on the original term with cutoff = 0 *)
		

	(* Defines substitution operation. *)
	(* 6.2.4 pag 80 *)
	let termSubst j s t =  (* [j -> s]t *)
		let rec walk c t = 
			match t with
			| TmVar(x, n) -> if x = j + c   					(* if x is exactly j plus the cutoff *)
							then termShift c s      			(* all the j in t are substituted with s *)
							else TmVar(x, n)					(* nothing happens *)
			| TmAbs(x, t1) -> TmAbs(x, walk (c + 1) t1)			(* call the walk function increasing the cutoff *)
			| TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)		(* call the walk on the two subterms *)
		in walk 0 t

	
	let termSubstTop s t =
		termShift (-1) (termSubst 0 (termShift 1 s) t)				(* the term being substituted for the bound variable is first shifted up by one, 
															then the substitution is made, and then the resulting term is shifted down because
															a bound variable has been used *)
	
	
	
	
	(* Note that, in the pure lambda-calculus, lambda-abstractions are the only
	possible values, so if we reach a state where E-App1 has succeeded in reducing
	t 1 to a value, then this value must be a lambda-abstraction. This observation 
	fails, of course, when we add other constructs such as primitive booleans to
	the language, since these introduce forms of values other than abstractions.*)
	let rec isval ctx t = 
		match t with
		| TmAbs(_, _) -> true
		| _ -> false


	(*single step evaluation*)	
	let rec eval1 ctx t = 
		match t with
		| TmApp(TmAbs(x, t12), v2) when isval ctx v2 -> termSubstTop v2 t12             			(*E_APPABS*)
		| TmApp(v1, t2) when isval ctx v1 -> let t2' = eval1 ctx t2 in TmApp(v1, t2')     			(*E_APP2*)
		| TmApp(t1, t2) -> let t1' = eval1 ctx t1 in TmApp(t1', t2)                                 	(*E_APP1*)
		| _ -> raise NoRuleApplies
		
		
		
	(*single step evaluation + printing*)	
	let rec printed_eval1 ctx t = 
		match t with
		| TmApp(TmAbs(x, t12), v2) when isval ctx v2 -> Format.printf "E_APPABS \n"; termSubstTop v2 t12            			 (*E_APPABS*)
		| TmApp(v1, t2) when isval ctx v1 -> Format.printf "E_APP2 \n"; let t2' = printed_eval1 ctx t2 in TmApp(v1, t2')     	 (*E_APP2*)
		| TmApp(t1, t2) -> Format.printf "E_APP1 \n"; let t1' = printed_eval1 ctx t1 in TmApp(t1', t2)                          (*E_APP1*)
		| _  ->  raise NoRuleApplies
		

		
	(*multistep evaluation*)	
	let rec eval ctx t =
		try let t' = eval1 ctx t in eval ctx t'
	with NoRuleApplies -> t
	
	
	let rec printed_eval ctx t =
		try let t' = printed_eval1 ctx t in printed_eval ctx t'
	with NoRuleApplies -> t
	
	
	
	(* big-step evaluation *)
	let rec bigstep ctx t =
		match t with	
		| TmAbs(x, t) -> TmAbs(x, t)
		| TmApp(t1, t2) -> let t1' = bigstep ctx t1 in
						(match t1' with
						| TmAbs(x, t12) -> let v2 = bigstep ctx t2 in
										let t' = bigstep ctx (termSubstTop v2 t12) in
										t'
						| _ -> raise NoRuleApplies
						
						)
		| _ -> raise NoRuleApplies
		
		
	let rec printed_bigstep ctx t =
		match t with	
		| TmAbs(x, t) -> Format.printf "B-ABS\n"; TmAbs(x, t)
		| TmApp(t1, t2) -> let t1' = printed_bigstep ctx t1 in
						(match t1' with
						| TmAbs(x, t12) -> Format.printf "B-APPABS\n"; let v2 = printed_bigstep ctx t2 in
										let t' = printed_bigstep ctx (termSubstTop v2 t12) in
										t'
						| _ -> raise NoRuleApplies
						
						)
		| _ -> raise NoRuleApplies


		
		
	let multi_vs_Bigstep ctx t = 
		Format.printf "----------------------------------------------\n";
		Format.printf "THE INSERTED TERM IS:\n%s\n" (to_string ctx t); 
		Format.printf "---------------------------------------------\n\n";
		Format.printf "*** THE BIG STEP EVALUATION IS: \n\n";
		let tBig = printed_bigstep ctx t in
		Format.printf "\nThe result of the Big-step evaluation is: %s\n\n" (to_string ctx tBig);
		Format.printf "*** THE MULTI STEP EVALUATION IS: \n\n";
		let tMulti = printed_eval ctx t in 
		Format.printf "\nThe result of the Multi-step evaluation is: %s\n\n" (to_string ctx tMulti)
	
	
	
	
	(* Church booleans *)
	let tru = TmAbs("t", TmAbs("f", TmVar(1, 2)))
	let fls = TmAbs("t", TmAbs("f", TmVar(0, 2)))
	
	let orr = TmAbs("b", TmAbs("c", TmApp(TmApp(TmVar(1, 2), (termShift 2 tru)), TmVar(0, 2))))
	let andd = TmAbs("b", TmAbs("c", TmApp(TmApp(TmVar(1, 2), TmVar(0, 2)), (termShift 2 fls))))
	let nott = TmAbs("a", TmApp(TmApp(TmVar(0, 1), (termShift 1 fls)), (termShift 1 tru)))
	
	
	let c0 = TmAbs("s", TmAbs("z", TmVar(0, 2)))
	let c1 = TmAbs("s", TmAbs("z", TmApp(TmVar(1, 2), TmVar(0, 2))))
	
	
end;;


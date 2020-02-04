module Term = 


	struct
	
		exception NoRuleApplies of string					(* Defining the NoRuleApplies exception. *)

		type term = 									(* Defining the term set, which contains all the possible terms*)
			| TmTrue   									(* Constant true *)
			| TmFalse   								(* Constant false *)
			| TmIf     of term * term * term 				(* Conditional *)
			| TmZero    								(* Constant zero *)
			| TmSucc   of term 							(* Successor *)
			| TmPred   of term 							(* Predecessor *)
			| TmIsZero of term							(* Zero test *)
			| TmVar of int * int       					(* int1 : de Brujin index     int2: actual size of the context *)
			| TmAbs of string * term					(* string: name of the variable in the ordinary representation *)
			| TmApp of term * term		
			
		type binding = NameBind
		
		type context = (string * binding) list		(* context represented as a list of strings and associated bindings *)
		
		
		
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
			| t when isnumericval t -> true 
			| TmAbs(_, _) -> true
			| _ -> false
			
			
		
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




		(* Single step evaluation *)
		let rec eval1 ctx t = 
			match t with
			| TmIf(TmTrue , t2, t3) -> t2                                             				(*E_iftrue*)
			| TmIf(TmFalse, t2, t3) -> t3 												(*E_iffalse*)	
			| TmIf(t1, t2, t3) -> let t1' = eval1 ctx t1 in TmIf(t1', t2, t3)						(*E_if*)
			| TmSucc(t1) -> let t1' = eval1 ctx t1 in TmSucc (t1')                      				(*E_succ*)
			| TmPred(TmZero) -> TmZero													(*E_predzero*)
			| TmPred(TmSucc(nv1)) when (isnumericval nv1) -> nv1								(*E_predsucc*)
			| TmPred(t1) -> let t1' = eval1 ctx t1 in TmPred(t1')									(*E_pred*)
			| TmIsZero (TmZero) -> TmTrue 												(*E_iszerozero*)
			| TmIsZero (TmSucc(nv1)) when (isnumericval nv1) -> TmFalse					 		(*E_iszerosucc*)
			| TmIsZero (t1) -> let t1' = eval1 ctx t1 in TmIsZero(t1')								(*E_iszero*)	
			| TmApp(TmAbs(x, t12), v2) when isval v2 -> termSubstTop v2 t12             			(*E_APPABS*)
			| TmApp(v1, t2) when isval v1 -> let t2' = eval1 ctx t2 in TmApp(v1, t2')     			(*E_APP2*)
			| TmApp(t1, t2) -> let t1' = eval1 ctx t1 in TmApp(t1', t2)                                 	(*E_APP1*)
			| _ -> raise (NoRuleApplies "")												



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
			| TmApp(TmAbs(x, t12), v2) when isval v2 -> Format.printf "E_APPABS \n"; termSubstTop v2 t12            			 (*E_APPABS*)
			| TmApp(v1, t2) when isval v1 -> Format.printf "E_APP2 \n"; let t2' = printEvalStep ctx t2 in TmApp(v1, t2')     	 (*E_APP2*)
			| TmApp(t1, t2) -> Format.printf "E_APP1 \n"; let t1' = printEvalStep ctx t1 in TmApp(t1', t2)                          (*E_APP1*)
			| _ -> raise (NoRuleApplies "")		
			)  
			 
		

		(* Multi-step evaluation *)
		let rec eval ctx t = 
			try let t' = eval1 ctx t in eval ctx t'
			with NoRuleApplies "" -> t

		(* Printed multi-step evaluation *)
		let rec printedEval ctx t =
			try let t' = printEvalStep ctx t in printedEval ctx t'
			with NoRuleApplies "" -> t
			
	


		(* Big-step evaluation *)
		let rec bigstep ctx t =
			match t with 
	 			| TmIf(t1,t2,t3) -> let t1' = bigstep ctx t1 in       
						(match t1' with
						| TmTrue -> let v2 = bigstep ctx t2 in   		 			 		(*B_IfTrue*)
						v2
						| TmFalse -> let v3 = bigstep ctx t3 in	 				     	(*B_IfFalse*)
						v3
						| _ -> raise (NoRuleApplies "Invalid argument. TmIf accepts only TmTrue and TmFalse as argument.")
						)
			| TmSucc(t1) -> let nv1 = bigstep ctx t1 in								 		(*B_Succ*)
						if isnumericval nv1 
						then TmSucc(nv1)
						else raise (NoRuleApplies "Invalid argument. TmSucc accepts only numeric values as argument.")
			| TmPred(t1) -> let t1' = bigstep ctx t1 in 
						(match t1' with 
						| TmZero -> t1'									         	(*B_PredZero*)
						| TmSucc(nv1) when (isnumericval nv1) -> nv1		     			(*B_PredSucc*)
						| _ -> raise (NoRuleApplies "Invalid argument. TmPred accepts only numeric values as argument.")
						)
			| TmIsZero(t1) -> let t1' = bigstep ctx t1 in 
						(match t1' with 
						| TmZero -> TmTrue									     	(*B_IsZeroZero*)
						| TmSucc(nv1) when (isnumericval nv1) -> TmFalse		 			(*B_IsZeroSucc*)
						| _ -> raise (NoRuleApplies "Invalid argument. TmZero accepts only numeric values as argument.")
						)
			| TmAbs(x, t) -> TmAbs(x, t)
			| TmApp(t1, t2) -> let t1' = bigstep ctx t1 in
						(match t1' with
						| TmAbs(x, t12) -> let v2 = bigstep ctx t2 in
										let t' = bigstep ctx (termSubstTop v2 t12) in
										t'
						| _ -> raise (NoRuleApplies "")
						
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
						| _ -> raise (NoRuleApplies "Invalid argument. TmIf accepts only TmTrue and TmFalse as argument.")
						)
			| TmSucc(t1) -> let nv1 = printedBigstep ctx t1 in												(*B_Succ*)
						if isnumericval nv1 
						then (Format.printf " B_Succ \n"; TmSucc(nv1))
						else raise (NoRuleApplies "Invalid argument. TmSucc accepts only numeric values as argument.")
			| TmPred(t1) -> let t1' = printedBigstep ctx t1 in 
						(match t1' with 
						| TmZero -> Format.printf " B_PredZero \n"; t1'									(*B_PredZero*)
						| TmSucc(nv1) when (isnumericval nv1) -> Format.printf " B_PredSucc \n"; nv1		     (*B_PredSucc*)
						| _ -> raise (NoRuleApplies "Invalid argument. TmPred accepts only numeric values as argument.")
						)
			| TmIsZero(t1) -> let t1' = printedBigstep ctx t1 in 
						(match t1' with 
						| TmZero -> Format.printf " B_IsZeroZero \n"; TmTrue								(*B_IsZeroZero*)
						| TmSucc(nv1) when (isnumericval nv1) -> Format.printf " B_IsZeroSucc \n"; TmFalse		(*B_IsZeroSucc*)
						| _ -> raise (NoRuleApplies "Invalid argument. TmZero accepts only numeric values as argument.")
						)
			| TmAbs(x, t) -> Format.printf "B-ABS\n"; TmAbs(x, t)
			| TmApp(t1, t2) -> let t1' = printedBigstep ctx t1 in
							(match t1' with
							| TmAbs(x, t12) -> Format.printf "B-APPABS\n"; let v2 = printedBigstep ctx t2 in
											let t' = printedBigstep ctx (termSubstTop v2 t12) in
											t'
							| _ -> raise (NoRuleApplies "")

							)



		(* Function that returns a string representation of a term *)
		let rec to_string ctx t =
			match t with 
			| TmTrue -> "True"
			| TmFalse -> "False"
			| TmIf(t1, t2, t3) -> "If (" ^ to_string ctx t1 ^ ") then (" ^ to_string ctx t2 ^ ") else (" ^ to_string ctx t3 ^ ")"
			| TmZero -> "0"
			| TmSucc(nv) -> "Succ(" ^ to_string ctx nv ^ ")"
			| TmPred(nv) -> "Pred(" ^ to_string ctx nv ^ ")"
			| TmIsZero(nv) -> "IsZero(" ^ to_string ctx nv ^ ")"
			| TmAbs(x,t1) -> let (ctx',x') = pickfreshname ctx x in						(* A new name is chosen for x if it already exists *)
							"(λ " ^ x' ^ ". " ^ (to_string ctx' t1) ^ ")"
			| TmApp(t1, t2) -> "(" ^ to_string ctx t1 ^ " " ^ to_string ctx t2 ^ ")"
			| TmVar(x,n) -> if ctxlength ctx = n then									(* Consistency check. If it doesn't pass a shift is missing somewhere *)
							(index2name ctx x)					     					(* return the name corresponding to the index in ctx *)
							else
							let ctxl = ctxlength ctx in 
							"[bad index, n is " ^ (string_of_int n) ^ " and length is " ^ (string_of_int ctxl) ^ "]"
							



		(* Function that prints a comparison between big-step and multi-step evaluation styles *)
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




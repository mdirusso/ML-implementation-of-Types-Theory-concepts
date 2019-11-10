(*An ML Implementation of Arithmetic Expressions*)

module Term = 


	struct
	
		exception NoRuleApplies

		type term = 
			| TmTrue   
			| TmFalse   
			| TmIf     of term * term * term 
			| TmZero    
			| TmSucc   of term 
			| TmPred   of term 
			| TmIsZero of term
			
			
			

	let rec isnumericval t =
		match t with 
		| TmZero -> true 
		| TmSucc(t1) -> isnumericval t1 
		| _ -> false


		
	let rec isval t = 
		match t with 
		| TmTrue -> true
		| TmFalse -> true
		| t when isnumericval t -> true 
		| _ -> false



	let rec eval1 t = 
		match t with
		| TmIf(TmTrue , t2, t3) -> t2                                             				(*E_iftrue*)
		| TmIf(TmFalse, t2, t3) -> t3 												(*E_iffalse*)	
		| TmIf(t1, t2, t3) -> let t1' = eval1 t1 in TmIf(t1', t2, t3)						(*E_if*)
		| TmSucc(t1) -> let t1' = eval1 t1 in TmSucc (t1')                      				(*E_succ*)
		| TmPred(TmZero) -> TmZero													(*E_predzero*)
		| TmPred(TmSucc(nv1)) when (isnumericval nv1) -> nv1								(*E_predsucc*)
		| TmPred(t1) -> let t1' = eval1 t1 in TmPred(t1')									(*E_pred*)
		| TmIsZero (TmZero) -> TmTrue 												(*E_iszerozero*)
		| TmIsZero (TmSucc(nv1)) when (isnumericval nv1) -> TmFalse					 		(*E_iszerosucc*)
		| TmIsZero (t1) -> let t1' = eval1 t1 in TmIsZero(t1')								(*E_iszero*)	
		| _ -> raise NoRuleApplies 
		
	(* simulare più small step per ottenere risulatato di bigstep*)

	(*cap 3 pagina 43*) 
	let rec bigstep t =
		match t with 
		| TmFalse | TmTrue | TmZero -> t		           					(*B_Value*)
		| TmIf(t1,t2,t3) -> let t1' = bigstep t1 in       
					(match t1' with
					| TmTrue -> let v2 = bigstep t2 in   		 			(*B_IfTrue*)
					v2
					| TmFalse -> let v3 = bigstep t3 in	 				(*B_IfFalse*)
					v3
					| _ -> raise NoRuleApplies
					)
		| TmSucc(t1) -> let nv1 = bigstep t1 in								(*B_Succ*)
					if isnumericval nv1 
					then TmSucc(nv1)
					else raise NoRuleApplies
		| TmPred(t1) -> let t1' = bigstep t1 in 
					(match t1' with 
					| TmZero -> t1'									(*B_PredZero*)
					| TmSucc(nv1) when (isnumericval nv1) -> nv1				(*B_PredSucc*)
					| _ -> raise NoRuleApplies
					)
		| TmIsZero(t1) -> let t1' = bigstep t1 in 
					(match t1' with 
					| TmZero -> TmTrue									(*B_IsZeroZero*)
					| TmSucc(nv1) when (isnumericval nv1) -> TmFalse			(*B_IsZeroSucc*)
					| _ -> raise NoRuleApplies
					)
			

			
	let rec eval t =
		try let t' = eval1 t in eval t'
		with NoRuleApplies -> t

	
	
	let rec to_string t =
		match t with 
		| TmTrue -> "True"
		| TmFalse -> "False"
		| TmIf(t1, t2, t3) -> "If (" ^ to_string t1 ^ ") then (" ^ to_string t2 ^ ") else (" ^ to_string t3 ^ ")"
		| TmZero -> "0"
		| TmSucc(nv) -> "Succ(" ^ to_string nv ^ ")"
		| TmPred(nv) -> "Pred(" ^ to_string nv ^ ")"
		| TmIsZero(nv) -> "IsZero(" ^ to_string nv ^ ")"



	(* CAP 6 *)


	
	
	
	
	
end;;




(*An ML Implementation of Arithmetic Expressions*)

module Term = 


	struct
	
		exception NoRuleApplies of string					(* Defining the NoRuleApplies exception. *)

		type term = 									(* Defining the term set, which contains all the possible terms*)
			| TmTrue   								(* Constant true *)
			| TmFalse   								(* Constant false *)
			| TmIf     of term * term * term 				(* Conditional *)
			| TmZero    								(* Constant zero *)
			| TmSucc   of term 							(* Successor *)
			| TmPred   of term 							(* Predecessor *)
			| TmIsZero of term							(* Zero test *)
			
			
			

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
		| _ -> false


	(* Single step evaluation *)
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
		| _ -> raise (NoRuleApplies "")												
	
	(* Same behaviour as eval1 but prints the rule applied *)
	let printEvalStep t = 
		(match t with 
		| TmIf (TmTrue,t2,t3) -> Format.printf " E_iftrue \n"; t2                                   
		| TmIf (TmFalse,t2,t3) -> Format.printf " E_iffalse \n"; t3						
		| TmIf (t1,t2,t3) -> Format.printf " E_if \n"; let t1' = printEvalStep t1 in TmIf(t1', t2, t3)										
		| TmSucc (t1) ->  Format.printf " E_succ \n"; let t1' = printEvalStep t1 in TmSucc (t1')                       							
		| TmPred (TmZero) -> Format.printf " E_predzero \n "; TmZero 								    
		| TmPred (TmSucc(nv1)) when (isnumericval nv1) ->	Format.printf " E_predsucc \n"; nv1		
		| TmPred (t1) -> Format.printf " E_pred \n"; let t1' = printEvalStep t1 in TmPred(t1')							                    
		| TmIsZero (TmZero) -> Format.printf " E_iszerozero \n"; TmTrue						
		| TmIsZero (TmSucc(nv1)) when (isnumericval nv1) -> Format.printf " E_iszerosucc \n"; TmFalse  	
		| TmIsZero (t1) -> Format.printf " E_iszero \n"; let t1' = printEvalStep t1 in TmIsZero(t1')	
		| _ -> Format.printf ""		
         )  


	(* Multi-step evaluation *)
	let rec eval t = 
		try let t' = eval1 t in eval t'
		with NoRuleApplies "" -> t
	
	(* Printed multi-step evaluation *)
	let rec printedEval t =
		try let t' = printEvalStep t in printedEval t'			(* printedEval doesn't print when eval1 calls itself. Can do better*)
		with NoRuleApplies "" -> t
		
		
	(* Big-step evaluation *)
	let rec bigstep t =
		match t with 
		| TmFalse | TmTrue | TmZero -> t	           					 			(*B_Value*)
		| TmIf(t1,t2,t3) -> let t1' = bigstep t1 in       
					(match t1' with
					| TmTrue -> let v2 = bigstep t2 in   		 			 		(*B_IfTrue*)
					v2
					| TmFalse -> let v3 = bigstep t3 in	 				     	(*B_IfFalse*)
					v3
					| _ -> raise (NoRuleApplies "Invalid argument. TmIf accepts only TmTrue and TmFalse as argument.")
					)
		| TmSucc(t1) -> let nv1 = bigstep t1 in								 		(*B_Succ*)
					if isnumericval nv1 
					then TmSucc(nv1)
					else raise (NoRuleApplies "Invalid argument. TmSucc accepts only numeric values as argument.")
		| TmPred(t1) -> let t1' = bigstep t1 in 
					(match t1' with 
					| TmZero -> t1'									         	(*B_PredZero*)
					| TmSucc(nv1) when (isnumericval nv1) -> nv1		     			(*B_PredSucc*)
					| _ -> raise (NoRuleApplies "Invalid argument. TmPred accepts only numeric values as argument.")
					)
		| TmIsZero(t1) -> let t1' = bigstep t1 in 
					(match t1' with 
					| TmZero -> TmTrue									     	(*B_IsZeroZero*)
					| TmSucc(nv1) when (isnumericval nv1) -> TmFalse		 			(*B_IsZeroSucc*)
					| _ -> raise (NoRuleApplies "Invalid argument. TmZero accepts only numeric values as argument.")
					)
	
	
	
	let rec printedBigstep t =
		match t with 
		| TmFalse | TmTrue | TmZero -> Format.printf " B_Value \n"; t	           						(*B_Value*)
		| TmIf(t1,t2,t3) -> let t1' = printedBigstep t1 in       
					(match t1' with
					| TmTrue -> Format.printf " B_IfTrue \n"; let v2 = printedBigstep t2 in 				(*B_IfTrue*)
					v2
					| TmFalse -> Format.printf " B_IfFalse \n"; let v3 = printedBigstep t3 in	 			(*B_IfFalse*)
					v3
					| _ -> raise (NoRuleApplies "Invalid argument. TmIf accepts only TmTrue and TmFalse as argument.")
					)
		| TmSucc(t1) -> let nv1 = printedBigstep t1 in												(*B_Succ*)
					if isnumericval nv1 
					then (Format.printf " B_Succ \n"; TmSucc(nv1))
					else raise (NoRuleApplies "Invalid argument. TmSucc accepts only numeric values as argument.")
		| TmPred(t1) -> let t1' = printedBigstep t1 in 
					(match t1' with 
					| TmZero -> Format.printf " B_PredZero \n"; t1'									(*B_PredZero*)
					| TmSucc(nv1) when (isnumericval nv1) -> Format.printf " B_PredSucc \n"; nv1		     (*B_PredSucc*)
					| _ -> raise (NoRuleApplies "Invalid argument. TmPred accepts only numeric values as argument.")
					)
		| TmIsZero(t1) -> let t1' = printedBigstep t1 in 
					(match t1' with 
					| TmZero -> Format.printf " B_IsZeroZero \n"; TmTrue								(*B_IsZeroZero*)
					| TmSucc(nv1) when (isnumericval nv1) -> Format.printf " B_IsZeroSucc \n"; TmFalse		(*B_IsZeroSucc*)
					| _ -> raise (NoRuleApplies "Invalid argument. TmZero accepts only numeric values as argument.")
					)
	

	
	(* Function that returns a string representation of a term *)
	let rec to_string t =
		match t with 
		| TmTrue -> "True"
		| TmFalse -> "False"
		| TmIf(t1, t2, t3) -> "If (" ^ to_string t1 ^ ") then (" ^ to_string t2 ^ ") else (" ^ to_string t3 ^ ")"
		| TmZero -> "0"
		| TmSucc(nv) -> "Succ(" ^ to_string nv ^ ")"
		| TmPred(nv) -> "Pred(" ^ to_string nv ^ ")"
		| TmIsZero(nv) -> "IsZero(" ^ to_string nv ^ ")"
	
		

	(* Function that prints a comparison between big-step and multi-step evaluation styles *)
	let multi_vs_Bigstep t = 
		Format.printf "----------------------------------------------\n";
		Format.printf "THE INSERTED TERM IS:\n%S\n" (to_string t); 
		Format.printf "---------------------------------------------\n\n";
		Format.printf "*** THE BIG STEP EVALUATION IS: \n\n";
		let tBig = printedBigstep t in
		Format.printf "\nThe result of the Big-step evaluation is: %s\n\n" (to_string tBig);
		Format.printf "*** THE MULTI STEP EVALUATION IS: \n\n";
		let tMulti = printedEval t in 
		Format.printf "\nThe result of the Multi-step evaluation is: %s\n\n" (to_string tMulti)



	
	
	
	
	
end;;




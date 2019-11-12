(*An ML Implementation of Arithmetic Expressions*)

module Term = 


	struct
	
		exception NoRuleApplies							(* Defining the NoRuleApplies exception. *)

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
		| _ -> raise NoRuleApplies
		
	let print t = 
		(match t with 
		| TmIf (TmTrue,t2,t3) -> Format.printf " E_iftrue \n"                                      
		| TmIf (TmFalse,t2,t3) -> 	Format.printf " E_iffalse \n " 										
		| TmIf (t1,t2,t3) -> Format.printf " E_if \n" 											
		| TmSucc (t1) ->  Format.printf " E_succ \n "                     							
		| TmPred (TmZero) -> Format.printf " E_predzero \n " 								    
		| TmPred (TmSucc(nv1)) when (isnumericval nv1) ->	Format.printf " E_predsucc \n" 			
		| TmPred (t1) -> Format.printf " E_pred \n" 							                    
		| TmIsZero (TmZero) -> Format.printf " E_iszerozero \n" 								
		| TmIsZero (TmSucc(nv1)) when (isnumericval nv1) -> Format.printf " E_iszerosucc \n"  	
		| TmIsZero (t1) -> Format.printf " E_iszero \n"
        | _ -> raise NoRuleApplies		
         )  

	(* Multi-step evaluation *)
	let rec eval t = 
		try let t' = Format.printf "the one step rule applied is: "; print t; eval1 t in eval t'
		with NoRuleApplies -> t
		
		
	
	
	

	(*cap 3 pagina 43*) 
	(* Big-step evaluation *)
	let rec bigstep t =
		match t with 
		| TmFalse | TmTrue | TmZero -> t	           					 (*B_Value*)
		| TmIf(t1,t2,t3) -> let t1' = bigstep t1 in       
					(match t1' with
					| TmTrue -> let v2 = bigstep t2 in   		 			 (*B_IfTrue*)
					v2
					| TmFalse -> let v3 = bigstep t3 in	 				     (*B_IfFalse*)
					v3
					| _ -> raise NoRuleApplies
					)
		| TmSucc(t1) -> let nv1 = bigstep t1 in								 (*B_Succ*)
					if isnumericval nv1 
					then TmSucc(nv1)
					else raise NoRuleApplies
		| TmPred(t1) -> let t1' = bigstep t1 in 
					(match t1' with 
					| TmZero -> t1'									         (*B_PredZero*)
					| TmSucc(nv1) when (isnumericval nv1) -> nv1		     (*B_PredSucc*)
					| _ -> raise NoRuleApplies
					)
		| TmIsZero(t1) -> let t1' = bigstep t1 in 
					(match t1' with 
					| TmZero -> TmTrue									     (*B_IsZeroZero*)
					| TmSucc(nv1) when (isnumericval nv1) -> TmFalse		 (*B_IsZeroSucc*)
					| _ -> raise NoRuleApplies
					)
	

	
    let printBig t = match t with
		| TmFalse | TmTrue | TmZero -> 	Format.printf " B_Value \n"           					
		| TmIf(t1,t2,t3) -> let t1' = bigstep t1 in       
					(match t1' with
					| TmTrue -> Format.printf " B_IfTrue \n" 		 			 
					
					| TmFalse -> Format.printf "B_IfFalse* \n" 				     
					
					| _ -> raise NoRuleApplies 
					)
		| TmSucc(t1) -> Format.printf "B_Succ \n" 
		| TmPred(t1) -> let t1' = bigstep t1 in 
					(match t1' with 
					| TmZero -> Format.printf "B_PredZero \n" 									         
					| TmSucc(nv1) when (isnumericval nv1) -> Format.printf "B_PredSucc \n" 		     
					| _ -> raise NoRuleApplies
					)
		| TmIsZero(t1) -> let t1' = bigstep t1 in 
					(match t1' with 
					| TmZero -> Format.printf "B_IsZeroZero \n" 								  
					| TmSucc(nv1) when (isnumericval nv1) -> Format.printf "B_IsZeroSucc \n" 				
					| _ -> raise NoRuleApplies
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
	
	
	
	
	(* Function that prints a term *)
	(*let printt t =
		Format.printf (to_string t)*)


    let eval_Bigstep t = 
		Format.printf "* THE BIG STEP EVALUATION IS: \n\n ";
		bigstep t; 
		Format.printf "the big step rule applied is: "; 
	    printBig t;
	    Format.printf "\n "


    let multi_vs_Bigstep t = 
	    Format.printf "----------------------------------------------\n";
		Format.printf "THE INSERT TERM IS:\n%S\n " (to_string t); 
		Format.printf "---------------------------------------------\n\n";
		eval_Bigstep t;
		Format.printf "* THE MULTI STEP EVALUATION IS : \n\n ";
		eval t



	
	
	
	
	
end;;




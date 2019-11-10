(*An ML Implementation of Arithmetic Expressions*)

module Project = 


	struct
	
		exception NoRuleApplies

		type info 
		type term = 
			| TmTrue   of info                          
			| TmFalse  of info 
			| TmIf     of info * term * term * term 
			| TmZero   of info 
			| TmSucc   of info * term 
			| TmPred   of info * term 
			| TmIsZero of info * term
			
			

	let rec isnumericval t = 
		match t with 
		| TmZero(_) -> true 
		| TmSucc(_,t1) -> isnumericval t1 
		| _ -> false

		
	let rec isval t = 
		match t with 
		| TmTrue(_) -> true
		| TmFalse(_) -> true
		| t when isnumericval t -> true 
		| _ -> false


	let rec eval1 t = 
		match t with
		| TmIf (_,TmTrue(_),t2,t3) -> t2                                             			(*E_iftrue*)
		| TmIf (_,TmFalse(_),t2,t3) -> t3 												(*E_iffalse*)	
		| TmIf (fi,t1,t2,t3) -> let t1' = eval1 t1 in TmIf(fi,t1',t2,t3)						(*E_if*)
		| TmSucc (fi,t1) -> let t1' = eval1 t1 in TmSucc(fi,t1')                      			(*E_succ*)
		| TmPred (_,TmZero(dummyinfo)) -> TmZero(dummyinfo)								(*E_predzero*)
		| TmPred (_,TmSucc(_,nv1)) when (isnumericval nv1) -> nv1							(*E_predsucc*)
		| TmPred (fi,t1) -> let t1' = eval1 t1 in TmPred(fi,t1')							(*E_pred*)
		| TmIsZero (_,TmZero(dummyinfo)) -> TmTrue (dummyinfo)								(*E_iszerozero*)
		| TmIsZero (_,TmSucc(dummyinfo,nv1)) when (isnumericval nv1) -> TmFalse (dummyinfo) 		(*E_iszerosucc*)
		| TmIsZero (fi,t1) -> let t1' = eval1 t1 in TmIsZero(fi,t1')						(*E_iszero*)	
		| _ -> raise NoRuleApplies 
		

	(*pagina 43 del libro*)
	let rec bigstep t =
		match t with 
		| TmFalse(_) | TmTrue(_) | TmZero(_) -> t           			(*B_Value*)
		| TmIf(_,t1,t2,t3) -> let t1' = bigstep t1 in       
						(match t1' with
						| TmTrue(_) -> let v2 = bigstep t2 in    	(*B_IfTrue*)
									v2
						| TmFalse(_) -> let v3 = bigstep t3 in	 	(*B_IfFalse*)
									v3
						| _ -> raise NoRuleApplies
						)
		| TmSucc(fi, t1) -> let nv1 = bigstep t1 in					(*B_Succ*)
					     if isnumericval nv1 
					     then TmSucc(fi, nv1)
					     else raise NoRuleApplies
		| TmPred(fi, t1) -> let t1' = bigstep t1 in 
						(match t1' with 
						| TmZero(_) -> t1'						(*B_PredZero*)
						| TmSucc(_, nv1) -> nv1					(*B_PredSucc*)
						| _ -> raise NoRuleApplies
						)
		| TmIsZero(fi, t1) -> let t1' = bigstep t1 in 
						(match t1' with 
						| TmZero(_) -> TmTrue(fi)				(*B_IsZeroZero*)
						| TmSucc(_, nv1) -> TmFalse(fi) 			(*B_IsZeroSucc*)
						| _ -> raise NoRuleApplies
						)
				
				
				
	let rec eval t =
		try let t' = eval1 t in eval t'
		with NoRuleApplies -> t
	(*let rec eval t = 
		try let t' = eval1 t in eval t'
		with NoRuleApplies -> t 
	*)


	
end;;


	
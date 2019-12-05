(*An ML Implementation of Simple Types*)

module Term = 

	struct 
		
		type info = int
		
		type ty =
		    | TyNat
			| TyBool
			| TyArr of ty * ty
		
		
		type binding =
			| NameBind
			| VarBind of ty
			
		type term =
			
			| TmTrue of info
			| TmFalse of info
			| TmZero of info
			| TmPred  of info * term
			| TmSucc of info * term
			| TmIsZero of info * term 
			| TmIf of info * term * term * term
			| TmVar of info * int * int
			| TmAbs of info * string * ty * term
			| TmApp of info * term * term
			
	type context = (string * binding) list		(* context represented as a list of strings and associated bindings *)
			
	
	let addbinding ctx x bind = (x,bind)::ctx   (*extend a context ctx with a new variable binding*)
	
	
	let getbinding fi ctx i = (*The getbinding function simply looks up the ith binding in the given context*)
	
	let getTypeFromContext fi ctx i =			(*extract the typing assumption associated with a particular variable i in a context ctx*)
				match getbinding fi ctx i with
					| VarBind(tyT) -> tyT
					| _ -> error fi ("getTypeFromContext: Wrong kind of binding for variable "^ (index2name fi ctx i))
	
	

	let rec to_string ctx t =  (*String conversion*)
		match t with 
		
		| TmTrue(fi) -> Format.printf"True"
		| TmFalse(fi) ->  Format.printf "False"
		| TmZero(fi) ->  Format.printf "0";
		| TmPred(fi,t) -> Format.printf "pred("; Format.printf"%s" to_string ctx t; Format.printf ")"
		| TmSucc(fi,t) -> Format.printf "succ(";Format.printf"%s" to_string ctx t ;Format.printf")"
		| TmIsZero(fi,t)  ->   Format.printf "isZero(" ;Format.printf"%s" to_string ctx t ;Format.printf")"
		| TmIf(fi,t1,t2,t3) -> Format.printf "If (";Format.printf"%s"to_string ctx t1 ; Format.printf ") then (";Format.printf"%s"to_string ctx t2; Format.printf") else (";Format.printf "%s"to_string ctx t3; Format.printf ")"
	
	
	(*Type checker*)
	let rec typeof ctx t =
		match t with
		| TmTrue(fi) -> TyBool
		
		| TmFalse(fi) -> TyBool
		
		| TmZero(fi) -> TyNat
		
		| TmPred(fi,t) ->  if (=) (typeof ctx t) TyNat then TyNat
									else error fi "t hasn't a predecessor"
		
		| TmSucc(fi,t) -> if (=) (typeof ctx t) TyNat then TyNat
									else error fi "t hasn't a successor"
									
		| TmIsZero(fi,t) -> error fi "not implemented"
		
		| TmIf(fi,t1,t2,t3) ->  if (=) (typeof ctx t1) TyBool then
									let tyT2 = typeof ctx t2 in
									if (=) tyT2 (typeof ctx t3) then tyT2
									else error fi "arms of conditional have different types"
								else error fi "guard of conditional not a boolean"
								
		| TmVar(fi,i,_) -> getTypeFromContext fi ctx i
		
		| TmAbs(fi,x,tyT1,t2) -> 	let ctx' = addbinding ctx x (VarBind(tyT1)) in
									let tyT2 = typeof ctx' t2 in
									TyArr(tyT1, tyT2)
									
		| TmApp(fi,t1,t2) -> 	let tyT1 = typeof ctx t1 in
								let tyT2 = typeof ctx t2 in
								(match tyT1 with
									| TyArr(tyT11,tyT12) -> if (=) tyT2 tyT11 then tyT12
															else error fi "parameter type mismatch"
									| _ -> error fi "arrow type expected"
								)
								
end;;
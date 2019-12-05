(*An ML Implementation of Simple Types*)

module Term = 

	struct 
		
		type binding =
			| NameBind
			| VarBind of ty
		
		type ty =
			| TyBool
			| TyArr of ty * ty
		
		type term =
			| TmTrue of info
			| TmFalse of info
			| TmIf of info * term * term * term
			| TmVar of info * int * int
			| TmAbs of info * string * ty * term			(* Type annotation on Abs *)
			| TmApp of info * term * term
			
	
	(* appends binding to the context *)
	let addbinding ctx x bind = (x, bind)::ctx
	
	let getTypeFromContext fi ctx i =
				match getbinding fi ctx i with
					| VarBind(tyT) -> tyT
					| _ -> error fi ("getTypeFromContext: Wrong kind of binding for variable " ^ (index2name fi ctx i))
	
	
	
	(* The typeof function uses a function addbinding to extend a context ctx
	with a new variable binding (x,bind) *)
	let rec typeof ctx t =
		match t with
		| TmTrue(fi) -> TyBool				(* true and false are bool *)
		| TmFalse(fi) -> TyBool
		
		| TmIf(fi,t1,t2,t3) ->  if (=) (typeof ctx t1) TyBool then					(* Checks if t1 is a bool, otw error *)
									let tyT2 = typeof ctx t2 in
									if (=) tyT2 (typeof ctx t3) then tyT2		(* Checks if t1 and t2 have same type, then this therm has same type of t2/t3, otw error *)
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
								
								
								
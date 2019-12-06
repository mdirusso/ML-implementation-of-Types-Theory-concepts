(*An ML Implementation of Subtyping*)

module Term = 

		type ty =
		| TyNat
		| TyBool
		| TyRecord of (string * ty) list
		| TyTop
		| TyArr of ty * ty
		
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
		| TmVar of iint * int
		| TmAbs of string * ty * term
		| TmApp of  term * term
		
		
		let rec subtype tyS tyT = 
			(=) tyS tyT || 
			match (tyS,tyT) with
				| (TyRecord(fS), TyRecord(fT)) ->  List.for_all
							(fun (li,tyTi) ->try let tySi = List.assoc li fS in
							subtype tySi tyTi  with Not_found -> false)
							fT
				| (_,TyTop)  -> true
				| (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->  (subtype tyT1 tyS1) && (subtype tyS2 tyT2)
				| (_,_) -> false
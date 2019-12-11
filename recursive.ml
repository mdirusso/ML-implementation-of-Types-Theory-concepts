module Term = 


		struct

		type ty =
			| TyNat
			| TyBool
			| TyRecord of (string * ty) list
			| TyTop
			| TyArr of ty * ty
			| TyUnit
			| TyPair of ty * ty
			| TyHungry of ty					(* A = Nat -> A *)
			| TyStream of ty					(* Unit -> {Nat,A} *)
			| TyCounter of ty 					(*  {get:Nat, inc:Unit -> C} *)
			

			
			
		type listnat = 
			| Nil of ty
			| Cons of ty * listnat
			
			
		
		

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
			
			
			
		let hd l = 
			match l with 
			| Nil(_) -> 0
			| Cons(n, l1) -> n
			
		let tl l = 
			match l with 
			| Nil(_) -> 0
			| Cons(n, l1) -> l1
			
			
		let isnil t =
			match t with 	
			| Nil(_) -> true
			| Cons(_, _) -> false
			
			
		(* s:NatList→Nat. λl:NatList. *)
		let sumlist s l = 
			if isnil l
			then 0
			else (hd l) + sumlist (tl l)
			
		
			
		
		
		
		
		
		
	end;;
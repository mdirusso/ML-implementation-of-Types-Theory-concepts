(*An ML Implementation of the Lambda-Calculus*)


module Term = 

	struct
	
		type info = int
		
		type term =
			| TmVar of info * int * int       		(* int1 : de Brujin index     int2: actual size of the context *)
			| TmAbs of info * string * term		(* string: name of the variable in the ordinary representation *)
			| TmApp of info * term * term			
			
			
		type binding = NameBind
		
		type context = (string * binding) list		(* context represented as a list of strings and associated bindings *)
		
		

	(* Function that returns the nameless representation of the term *)		
	(*let rec removenames ctx t =
		match t with 
		| TmVar(fi, x, ctx.length) -> (* prendo indice dell'ultima occorrenza di x in ctx *)
		| TmAbs(fi, str, ter) -> TmAbs(fi, str, removenames ctx ter)       (* Devo aggiungere str a ctx? *)      
		| TmApp(fi, t1, t2) -> TmApp(fi, removenames ctx t1, removenames ctx t2)
		
	
	(* Function that returns the ordinary representation of the term *)
	let rec restorenames ctx t =
		match t with 
		| TmVar(fi, k, ctx.length) -> (* il k-esimo elemento di ctx *)
		| TmAbs(fi, str, ter) -> TmAbs(fi, (*primo nome non in ctx *), restorenames ctx ter)          
		| TmApp(fi, t1, t2) -> TmApp(fi, restorenames ctx t1, restorenames ctx t2)*)
		
	
	
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
		then pickfreshname ctx (x ^ "'") 			(* add an apex to the name if it was already used *)
		else ((x, NameBind)::ctx), x				(* if the name is not already used, add it to ctx *)
		
		
				
	
	(*  *)
	let rec index2name fi ctx x =
		try
			let (name, _) = List.nth ctx x in
			name
		with Failure _ -> "[not found]"
			

	
	(* Given a context and a term, prints the term giving new names if they already exist in context *)
	let rec printtm ctx t = 
		match t with
		| TmAbs(fi,x,t1) -> let (ctx',x') = pickfreshname ctx x in		(* A new name is chosen for x if it already exists *)
						Format.printf "(lambda "; Format.printf "%s" (x'); Format.printf ". "; printtm ctx' t1; Format.printf ")"
		| TmApp(fi, t1, t2) -> Format.printf "("; printtm ctx t1; Format.printf " "; printtm ctx t2; Format.printf ")"
		| TmVar(fi,x,n) -> if ctxlength ctx = n then					(* Consistency check. If it doesn't pass a shift is missing somewhere *)
						Format.printf "%s" (index2name fi ctx x)	(* return the name corresponding to the index in ctx *)
						else
						Format.printf "[bad index]"
						
	
	
	
	(* Defines shifting operation *)
	(* 6.2.1 pag 79 *)	
	let termShift d t =
		let rec walk c t = 
			match t with
			| TmVar(fi, x, n) -> if x >= c 						(* The index has to be shifted only if it is greater than the cutoff *)
							then TmVar(fi, x + d, n + d)
							else TmVar(fi, x, n + d)
			| TmAbs(fi, x, t1) -> TmAbs(fi, x, walk (c + 1) t1)		(* Every time a bound is encountered, the cutoff increases by one and walk is called on the subterm *)
			| TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2)		(* In applications the shifting is applied on both subterms *)
		in walk 0 t											(* the first call is walk on the original term with cutoff = 0 *)
		
	
	
	(* Defines substitution operation. *)
	(* 6.2.4 pag 80 *)
	let termSubst j s t =  (* [j -> s]t *)
		let rec walk c t = 
			match t with
			| TmVar(fi, x, n) -> if x = j + c   					(* if x is exactly j plus the cutoff *)
							then termShift c s      				(* all the j in t are substituted with s *)
							else TmVar(fi, x, n)				(* nothing happens *)
			| TmAbs(fi, x, t1) -> TmAbs(fi, x, walk (c + 1) t1)		(* call the walk function increasing the cutoff *)
			| TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2)		(* call the walk on the two subterms *)
		in walk 0 t

	
	let termSubstTop s t =
		termShift (-1) (termSubst 0 (termShift 1 s) t)				(* the term being substituted for the bound variable is first shifted up by one, 
															then the substitution is made, and then the resulting term is shifted down because
															a bound variable has been used *)

end;;

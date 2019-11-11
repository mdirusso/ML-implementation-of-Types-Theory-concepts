(*An ML Implementation of the Lambda-Calculus*)


module Term = 

	struct
	
		type info
		
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
		| nil -> false
		| (name, _) :: l1 ->
			if name = x then true
			else alreadyused l1 x



	(* adds a name to the context, creating new name if already used *)
	let rec pickfreshname ctx x =
		if alreadyused ctx x 
		then pickfreshname ctx (x ^ "'") 			(* add a apex to the name if it was already used *)
		else ((x, NameBind)::ctx), x				(* if the name is not already used, add it to ctx *)
		
		
				
			
	let rec index2name fi ctx x =
		match List.nth ctx x with	
		| nil -> "[not found]"                     	(*  *)	
		| (name, _) -> name

		
	let rec printtm ctx t = 
		match t with
		| TmAbs(fi,x,t1) -> let (ctx',x') = pickfreshname ctx x in
			Format.printf "(lambda "; Format.printf "%s" (x'); Format.printf ". "; printtm ctx' t1; Format.printf ")"
		| TmApp(fi, t1, t2) -> Format.printf "("; printtm ctx t1; Format.printf " "; printtm ctx t2; Format.printf ")"
		| TmVar(fi,x,n) -> if ctxlength ctx = n then
						Format.printf "%s" (index2name fi ctx x)
						else
						Format.printf "[bad index]"


end;;
(*An ML Implementation of the Lambda-Calculus*)


module Term = 

	struct
	
		type info
		
		type term =
			| TmVar of info * int * int       (* int1 : de Brujin index     int2: actual size of the context*)
			| TmAbs of info * string * term
			| TmApp of info * term * term	
			
		type context = (string * binding) list
		
		type binding = NameBind


				
	let rec removenames ctx t =
		match t with 
		| TmVar(fi, x, ctx.length) -> (* prendo indice dell'ultima occorrenza di x in ctx *)
		| TmAbs(fi, str, ter) -> TmAbs(fi, str, removenames ctx ter)       (* Devo aggiungere str a ctx? *)      
		| TmApp(fi, t1, t2) -> TmApp(fi, removenames ctx t1, removenames ctx t2)
		

	let rec restorenames ctx t =
		match t with 
		| TmVar(fi, k, ctx.length) -> (* il k-esimo elemento di ctx *)
		| TmAbs(fi, str, ter) -> TmAbs(fi, (*primo nome non in ctx *), restorenames ctx ter)          
		| TmApp(fi, t1, t2) -> TmApp(fi, restorenames ctx t1, restorenames ctx t2)
		
	
	
	let rec printtm ctx t = 
		match t with
		| TmAbs(fi,x,t1) -> let (ctx',x') = pickfreshname ctx x in
									pr "(lambda "; pr x'; pr ". "; printtm ctx' t1; pr ")"
		| TmApp(fi, t1, t2) -> pr "("; printtm ctx t1; pr " "; printtm ctx t2; pr ")"
		| TmVar(fi,x,n) -> if ctxlength ctx = n then
						pr (index2name fi ctx x)
						else
						pr "[bad index]"


end;;
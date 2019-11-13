(*Example*)

(*Multi Step vs Big Step evaluation*)

# use "TER-Cap 4(noinfo).ml";;
open Term;;

let tIfTrue = TmIf (TmTrue, TmIsZero (TmSucc (TmZero)), TmTrue);;
multi_vs_Bigstep tIfTrue;;


let tIfFalse = TmIf (TmFalse, TmIsZero (TmSucc (TmZero)), TmTrue);;
multi_vs_Bigstep tIfFalse;;


let tWrong1 = TmIf (TmZero, TmIsZero (TmSucc (TmZero)), TmTrue);;
multi_vs_Bigstep tWrong1;;

let tWrong2 = TmIsZero (TmTrue);;
multi_vs_Bigstep tWrong2;;

let tWrong3 = TmIf (TmTrue, TmZero, TmTrue);;
multi_vs_Bigstep tWrong3;;

let tNested = TmIf(
			TmIf(TmIsZero(TmZero),
				TmIsZero(TmPred(TmSucc(TmZero))),
				TmFalse),
			TmIf(TmIsZero(TmSucc(TmZero)),
				TmZero,
				TmSucc(TmPred(TmSucc(TmSucc(TmZero))))),
			TmZero);;
multi_vs_Bigstep tNested;;


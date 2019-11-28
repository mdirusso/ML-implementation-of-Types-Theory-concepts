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


# use "TER-Cap 7(noinfo).ml";;
open Term;;
let ctx = [];;

(* iff = λl. λm. λn. l m n    *)
let iff = TmAbs("l", TmAbs("m", TmAbs("n", TmApp(TmApp(TmVar(2, 3), TmVar(1, 3)), TmVar(0, 3)))));;
let t = TmApp(iff, tru);;
let tf = TmApp(iff, fls);;
let iftrue = TmApp(TmApp(t, c0), c1);;
let iffalse = TmApp(TmApp(tf, c0), c1);;

let ortf = TmApp(TmApp(orr, tru), fls);;
let orff = TmApp(TmApp(orr, fls), fls);;

multi_vs_Bigstep ctx iftrue;;
multi_vs_Bigstep ctx iffalse;;
multi_vs_Bigstep ctx ortf;;
multi_vs_Bigstep ctx orff;;

printtm ctx (printed_eval ctx iftrue);;
printtm ctx (printed_bigstep ctx iftrue);;
printtm ctx (printed_eval ctx iffalse);;
printtm ctx (printed_bigstep ctx iffalse);;




(*λs. λz. z;
c1 = λs. λz. s z;*)

(* test = λl. λm. λn. l m n;; *)


let ex0 = TmApp(
			TmAbs("u", TmApp(TmVar(0, 1), TmVar(0, 1))),
			TmApp(
				TmAbs("x", TmApp(TmVar(0, 1), TmVar(0, 1))),
				TmApp(
					TmAbs("y", TmApp(TmVar(0, 1), TmVar(0, 1))),
					TmAbs("z", TmVar(0, 1)))));;


let a = 4;;
let b = a = a + b;;



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


let ctx = ("t", NameBind):: ("f", NameBind):: ("l", NameBind):: ("m", NameBind):: ("n", NameBind):: [];;
let ctx = [];;

let tru = TmAbs("t", TmAbs("f", TmVar(1, 2)));;
let fls = TmAbs("t", TmAbs("f", TmVar(0, 2)));;
let c0 = TmAbs("s", TmAbs("z", TmVar(0, 2)));;
let c1 = TmAbs("s", TmAbs("z", TmApp(TmVar(1, 2), TmVar(0, 2))));;

(*λs. λz. z;
c1 = λs. λz. s z;*)

(* test = λl. λm. λn. l m n;; *)
let test = TmAbs("l", TmAbs("m", TmAbs("n", TmApp(TmApp(TmVar(2, 3), TmVar(1, 3)), TmVar(0, 3)))));;
let t = TmApp(test, tru);;
let tf = TmApp(test, fls);;
let t11 = TmApp(TmApp(t, c0), c1);;
let t22 = TmApp(TmApp(tf, c0), c1);;

let ex0 = TmApp(
			TmAbs("u", TmApp(TmVar(0, 1), TmVar(0, 1))),
			TmApp(
				TmAbs("x", TmApp(TmVar(0, 1), TmVar(0, 1))),
				TmApp(
					TmAbs("y", TmApp(TmVar(0, 1), TmVar(0, 1))),
					TmAbs("z", TmVar(0, 1)))));;
let ex0 = TmApp(
			TmAbs("u", TmApp(TmVar(0, 1), TmVar(0, 1))),
			TmApp(
				TmAbs("x", TmApp(TmVar(0, 1), TmVar(0, 1))),
				TmApp(
					TmAbs("y", TmApp(TmVar(0, 1), TmVar(0, 1))),
					TmAbs("z", TmVar(0, 1)))));;

let a = 4;;
let b = a = a + b;;



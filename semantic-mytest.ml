
(*Tests for semantic-analyser.ml*)



#use "semantic-analyser.ml";;

open Semantics;;
open Tag_Parser;;
open Reader;;
open PC;;


(* Compare expr', add support for Box *)
let rec expr'_eq e1 e2 =


  match e1, e2 with
    | Const' Void, Const' Void -> true
    | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
    | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
    | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
    | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
    | Box' (v1), Box' (v2)  ->  expr'_eq (Var' v1) (Var' v2)
    | BoxGet' (v1), BoxGet' (v2)  ->  expr'_eq (Var' v1) (Var' v2)
    | BoxSet' (var1,val1), BoxSet' (var2 , val2)  ->  (expr'_eq (Var' var1) (Var' var2) && expr'_eq val1 val2)
    | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                              (expr'_eq th1 th2) &&
                                                (expr'_eq el1 el2)
    | (Seq'(l1), Seq'(l2)
    | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
    | (Set'(var1, val1), Set'(var2, val2)
    | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq var1 var2) &&
                                              (expr'_eq val1 val2)
    | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
      (List.length vars1) = (List.length vars2) &&
      (List.for_all2 String.equal vars1 vars2) &&
        (expr'_eq body1 body2)
    | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
      (String.equal var1 var2) &&
        (List.length vars1) = (List.length vars2) &&
        (List.for_all2 String.equal vars1 vars2) &&
          (expr'_eq body1 body2)
    | Applic'(e1, args1), Applic'(e2, args2)
    | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
    (expr'_eq e1 e2) &&
      (List.length args1) = (List.length args2) &&
      (List.for_all2 expr'_eq args1 args2)
    | _ -> false;;


exception X_no_match;;
exception X_syntax_error;;
exception X_output_fail of expr';;

type result = 
    |Fail of string * string * expr' * string * expr'
    |Success of int;;
     
(*Test will fail if an exception is raised,
or the output of parsing and analysing str is different than the expression out*)
let test_str str =
run_semantics str;;

let _assert num str out =
  try let result = test_str str in
      (if not (expr'_eq result out)
       then raise (X_output_fail result)
       else Success num)
  with
  |X_no_match ->
     (failwith
	(Printf.sprintf
	   "Failed %d with X_no_match: Reader couldn't parse the string "num))
  |X_output_fail result ->
     (Fail (
           (Printf.sprintf "********************FAILED %d********************" num),
           "********************EXPECTED OUTPUT********************",
           out, "********************ACTUAL OUTPUT********************",result))
  |X_syntax_error ->
     (failwith
	(Printf.sprintf
	   "Failed %d with X_syntax_error: Tag parser failed to resolve expression "num));;

_assert 1
(LambdaSimple ([], Const (Sexpr (Number (Int 1))))
)
(LambdaSimple' ([], Const' (Sexpr (Number (Int (1))))));;

_assert 2
(Const
  (Sexpr
    (Pair
      (Pair (Symbol "lambda",
        Pair (Nil,
         Pair
          (Pair (Symbol "lambda",
            Pair (Pair (Symbol "x", Nil),
             Pair (Symbol "x",
              Pair
               (Pair (Symbol "lambda",
                 Pair (Nil,
                  Pair
                   (Pair (Symbol "set!",
                     Pair (Symbol "x", Pair (Number (Int 1), Nil))),
                   Nil))),
               Nil)))),
          Nil))),
      Nil)))
)
(Const' (Sexpr (Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair (Symbol "x", Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Pair (Symbol "set!", Pair (Symbol "x", Pair (Number (Int (1)), Nil))), Nil))), Nil)))), Nil))), Nil))));;

_assert 3
(Applic
  (LambdaSimple (["x"],
    If (Applic (Var "x", [Const (Sexpr (Number (Int 1)))]),
     Applic (Var "x", [Const (Sexpr (Number (Int 2)))]),
     Applic
      (LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Int 0))))),
      [Const (Sexpr (Number (Int 3)))]))),
  [LambdaSimple (["x"], Var "x")])
)
(Applic' (LambdaSimple' (["x"], If' (Applic' (Var' (VarParam ("x", 0)), [Const' (Sexpr (Number (Int (1))))]), ApplicTP' (Var' (VarParam ("x", 0)), [Const' (Sexpr (Number (Int (2))))]), ApplicTP' (LambdaSimple' (["x"], Set' (Var' (VarParam ("x", 0)), Const' (Sexpr (Number (Int (0)))))), [Const' (Sexpr (Number (Int (3))))]))), [LambdaSimple' (["x"], Var' (VarParam ("x", 0)))]));;

_assert 4
(LambdaSimple (["x"],
  Or
   [Applic
     (LambdaOpt (["y"], "z",
       Applic
        (LambdaSimple ([],
          Applic (LambdaSimple ([], Applic (Var "+", [Var "x"; Var "z"])), [])),
        [])),
     [Var "x"; Const (Sexpr (Number (Int 1)))]);
    LambdaSimple ([], Set (Var "x", Var "w")); Applic (Var "w", [Var "w"])])
)
(LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Or' ([Applic' (LambdaOpt' (["y"], "z", ApplicTP' (LambdaSimple' ([], ApplicTP' (LambdaSimple' ([], ApplicTP' (Var' (VarFree "+"), [BoxGet' (VarBound ("x", 2, 0));Var' (VarBound ("z", 1, 1))])), [])), [])), [BoxGet' (VarParam ("x", 0));Const' (Sexpr (Number (Int (1))))]);LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), Var' (VarFree "w")));ApplicTP' (Var' (VarFree "w"), [Var' (VarFree "w")])])])));;

_assert 5
(If (Applic (LambdaSimple (["y"], Var "x"), []),
  Applic
   (LambdaSimple (["x"],
     Seq
      [Set (Var "x", Var "y");
       LambdaSimple ([], Set (Var "x", Const (Sexpr (Number (Int 1)))))]),
   [Const (Sexpr (Symbol "a"))]),
  LambdaSimple (["x"], Set (Var "x", Var "y")))
)
(If' (Applic' (LambdaSimple' (["y"], Var' (VarFree "x")), []), Applic' (LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Var' (VarFree "y"));LambdaSimple' ([], Set' (Var' (VarBound ("x", 0, 0)), Const' (Sexpr (Number (Int (1))))))])), [Const' (Sexpr (Symbol "a"))]), LambdaSimple' (["x"], Set' (Var' (VarParam ("x", 0)), Var' (VarFree "y")))));;

_assert 6
(LambdaOpt (["x"; "y"; "z"], "w",
  Seq
   [Var "z";
    Applic
     (LambdaSimple ([],
       Seq [Set (Var "w", Var "w"); Applic (Applic (Var "y", [Var "x"]), [])]),
     [])])
)
(LambdaOpt' (["x";"y";"z"], "w", Seq' ([Var' (VarParam ("z", 2));ApplicTP' (LambdaSimple' ([], Seq' ([Set' (Var' (VarBound ("w", 0, 3)), Var' (VarBound ("w", 0, 3)));ApplicTP' (Applic' (Var' (VarBound ("y", 0, 1)), [Var' (VarBound ("x", 0, 0))]), [])])), [])])));;

_assert 7
(Def (Var "a",
  Applic
   (LambdaSimple ([],
     LambdaOpt ([], "x",
      Seq
       [Var "x";
        LambdaOpt ([], "y", Set (Var "y", Const (Sexpr (Number (Int 1)))))])),
   []))
)
(Def' (Var' (VarFree "a"), Applic' (LambdaSimple' ([], LambdaOpt' ([], "x", Seq' ([Var' (VarParam ("x", 0));LambdaOpt' ([], "y", Set' (Var' (VarParam ("y", 0)), Const' (Sexpr (Number (Int (1))))))]))), [])));;

_assert 8
(LambdaSimple (["x"; "y"],
  Seq
   [Applic (Var "x", [Var "y"]);
    LambdaSimple ([],
     LambdaSimple ([],
      LambdaSimple ([],
       Set (Var "x",
        Applic (LambdaSimple (["z"], Set (Var "y", Var "x")), [Var "y"])))))])
)
(LambdaSimple' (["x";"y"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Set' (Var' (VarParam ("y", 1)), Box' (VarParam ("y", 1)));Seq' ([Applic' (BoxGet' (VarParam ("x", 0)), [BoxGet' (VarParam ("y", 1))]);LambdaSimple' ([], LambdaSimple' ([], LambdaSimple' ([], BoxSet' (VarBound ("x", 2, 0), Applic' (LambdaSimple' (["z"], BoxSet' (VarBound ("y", 3, 1), BoxGet' (VarBound ("x", 3, 0)))), [BoxGet' (VarBound ("y", 2, 1))])))))])])));;

_assert 9
(LambdaSimple ([],
  Seq
   [Applic (LambdaSimple ([], Var "x"), []);
    Applic
     (LambdaSimple (["x"],
       Seq
        [Set (Var "x", Const (Sexpr (Number (Int 1))));
         LambdaSimple ([], Var "x")]),
     [Const (Sexpr (Number (Int 2)))]);
    Applic (LambdaOpt ([], "x", Var "x"), [Const (Sexpr (Number (Int 3)))])])
)
(LambdaSimple' ([], Seq' ([Applic' (LambdaSimple' ([], Var' (VarFree "x")), []);Applic' (LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Seq' ([BoxSet' (VarParam ("x", 0), Const' (Sexpr (Number (Int (1)))));LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)))])])), [Const' (Sexpr (Number (Int (2))))]);ApplicTP' (LambdaOpt' ([], "x", Var' (VarParam ("x", 0))), [Const' (Sexpr (Number (Int (3))))])])));;

_assert 10
(LambdaSimple (["x"; "y"; "z"],
  Seq
   [LambdaSimple (["y"],
     Seq
      [Set (Var "x", Const (Sexpr (Number (Int 5))));
       Applic (Var "+", [Var "x"; Var "y"])]);
    Applic (Var "+", [Var "x"; Var "y"; Var "z"])])
)
(LambdaSimple' (["x";"y";"z"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Seq' ([LambdaSimple' (["y"], Seq' ([BoxSet' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Int (5)))));ApplicTP' (Var' (VarFree "+"), [BoxGet' (VarBound ("x", 0, 0));Var' (VarParam ("y", 0))])]));ApplicTP' (Var' (VarFree "+"), [BoxGet' (VarParam ("x", 0));Var' (VarParam ("y", 1));Var' (VarParam ("z", 2))])])])));;

_assert 11
(LambdaSimple (["x"], Set (Var "x", Applic (LambdaSimple ([], Var "x"), [])))
)
(LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));BoxSet' (VarParam ("x", 0), Applic' (LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0))), []))])));;

_assert 12
(Applic (Var "y",
  [LambdaSimple (["y"],
    Seq
     [Set (Var "a", LambdaSimple (["b"], Applic (Var "a", [Var "b"])));
      Set (Var "t",
       LambdaSimple (["x"],
        Seq
         [Set (Var "y",
           LambdaSimple (["j"], Applic (Var "x", [Var "j"; Var "x"])));
          Var "h"]));
      Applic (Var "y", [Var "a"])])])
)
(Applic' (Var' (VarFree "y"), [LambdaSimple' (["y"], Seq' ([Set' (Var' (VarParam ("y", 0)), Box' (VarParam ("y", 0)));Seq' ([Set' (Var' (VarFree "a"), LambdaSimple' (["b"], ApplicTP' (Var' (VarFree "a"), [Var' (VarParam ("b", 0))])));Set' (Var' (VarFree "t"), LambdaSimple' (["x"], Seq' ([BoxSet' (VarBound ("y", 0, 0), LambdaSimple' (["j"], ApplicTP' (Var' (VarBound ("x", 0, 0)), [Var' (VarParam ("j", 0));Var' (VarBound ("x", 0, 0))])));Var' (VarFree "h")])));ApplicTP' (BoxGet' (VarParam ("y", 0)), [Var' (VarFree "a")])])]))]));;

_assert 13
(LambdaSimple (["x"],
  Seq
   [LambdaSimple (["x"], Set (Var "x", Var "x"));
    LambdaSimple (["x"], Set (Var "x", Var "x"))])
)
(LambdaSimple' (["x"], Seq' ([LambdaSimple' (["x"], Set' (Var' (VarParam ("x", 0)), Var' (VarParam ("x", 0))));LambdaSimple' (["x"], Set' (Var' (VarParam ("x", 0)), Var' (VarParam ("x", 0))))])));;

_assert 14
(LambdaSimple (["x"; "y"],
  Seq
   [LambdaSimple ([], Set (Var "x", Var "y"));
    LambdaSimple ([], Set (Var "y", Var "x"))])
)
(LambdaSimple' (["x";"y"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Set' (Var' (VarParam ("y", 1)), Box' (VarParam ("y", 1)));Seq' ([LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), BoxGet' (VarBound ("y", 0, 1))));LambdaSimple' ([], BoxSet' (VarBound ("y", 0, 1), BoxGet' (VarBound ("x", 0, 0))))])])));;

_assert 15
(LambdaOpt ([], "x",
  Seq
   [LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Int 1)))));
    Applic (Var "car", [Var "x"])])
)
(LambdaOpt' ([], "x", Seq' ([LambdaSimple' (["x"], Set' (Var' (VarParam ("x", 0)), Const' (Sexpr (Number (Int (1))))));ApplicTP' (Var' (VarFree "car"), [Var' (VarParam ("x", 0))])])));;

_assert 16
(If (Var "x", Applic (Var "x", []), Var "x")
)
(If' (Var' (VarFree "x"), Applic' (Var' (VarFree "x"), []), Var' (VarFree "x")));;

_assert 17
(LambdaSimple ([],
  If (Var "x", Applic (Var "x", []), Applic (Var "not", [Var "x"])))
)
(LambdaSimple' ([], If' (Var' (VarFree "x"), ApplicTP' (Var' (VarFree "x"), []), ApplicTP' (Var' (VarFree "not"), [Var' (VarFree "x")]))));;

_assert 18
(LambdaSimple (["a"; "b"; "c"; "d"; "e"],
  Applic (Var "a",
   [Applic (Var "b", [Var "c"]); Applic (Var "c", [Var "b"; Var "d"]);
    Applic (Var "a",
     [Applic (Var "b", [Applic (Var "c", [Applic (Var "d", [Var "e"])])])])]))
)
(LambdaSimple' (["a";"b";"c";"d";"e"], ApplicTP' (Var' (VarParam ("a", 0)), [Applic' (Var' (VarParam ("b", 1)), [Var' (VarParam ("c", 2))]);Applic' (Var' (VarParam ("c", 2)), [Var' (VarParam ("b", 1));Var' (VarParam ("d", 3))]);Applic' (Var' (VarParam ("a", 0)), [Applic' (Var' (VarParam ("b", 1)), [Applic' (Var' (VarParam ("c", 2)), [Applic' (Var' (VarParam ("d", 3)), [Var' (VarParam ("e", 4))])])])])])));;

_assert 19
(LambdaSimple (["x"],
  Seq [Applic (Var "x", []); Set (Var "x", Applic (Var "x", []))])
)
(LambdaSimple' (["x"], Seq' ([Applic' (Var' (VarParam ("x", 0)), []);Set' (Var' (VarParam ("x", 0)), Applic' (Var' (VarParam ("x", 0)), []))])));;

_assert 20
(LambdaSimple (["x"],
  Applic
   (LambdaSimple (["y"],
     Seq [Set (Var "x", Applic (Var "y", [])); Const (Sexpr (Number (Int 2)))]),
   []))
)
(LambdaSimple' (["x"], ApplicTP' (LambdaSimple' (["y"], Seq' ([Set' (Var' (VarBound ("x", 0, 0)), Applic' (Var' (VarParam ("y", 0)), []));Const' (Sexpr (Number (Int (2))))])), [])));;

_assert 21
(Const(Void)
)
(Const' (Void));;

_assert 22
(LambdaSimple (["x"],
  Seq
   [Var "x";
    LambdaSimple (["x"],
     Seq
      [Set (Var "x", Const (Sexpr (Number (Int 1))));
       LambdaSimple ([], Var "x")]);
    LambdaSimple ([], Set (Var "x", Var "x"))])
)
(LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Seq' ([BoxGet' (VarParam ("x", 0));LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Seq' ([BoxSet' (VarParam ("x", 0), Const' (Sexpr (Number (Int (1)))));LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)))])]));LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), BoxGet' (VarBound ("x", 0, 0))))])])));;

_assert 23
(LambdaSimple (["x"],
  Seq
   [Var "x";
    LambdaSimple (["x"],
     Seq
      [Set (Var "y", Var "x");
       LambdaSimple ([], Var "x")]);
    LambdaSimple ([], Set (Var "x", Var "x"))])
)
(LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Seq' ([BoxGet' (VarParam ("x", 0));LambdaSimple' (["x"], Seq' ([Set' (Var' (VarFree "y"), Var' (VarParam ("x", 0)));LambdaSimple' ([], Var' (VarBound ("x", 0, 0)))]));LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), BoxGet' (VarBound ("x", 0, 0))))])])));;

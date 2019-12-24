
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
run_semantics (tag_parse_expression (read_sexpr str));;

let _assert num str out =
  try let result = test_str str in
      (if not (expr'_eq result out)
       then raise (X_output_fail result)
       else Success num)
  with
  |X_no_match ->
     (failwith
	(Printf.sprintf
	   "Failed %d with X_no_match: Reader couldn't parse the string '%s'"num str))
  |X_output_fail result ->
     (Fail (
           (Printf.sprintf "********************FAILED %d********************" num),
           "********************EXPECTED OUTPUT********************",
           out, "********************ACTUAL OUTPUT********************",result))
  |X_syntax_error ->
     (failwith
	(Printf.sprintf
	   "Failed %d with X_syntax_error: Tag parser failed to resolve expression '%s'"num str));;

_assert 1 " (define t (lambda (x) (lambda () x) (lambda () (set! x 1))))"
(Def' (Var' (VarFree "t"),
  LambdaSimple' (["x"],
   Seq'
    [Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
     Seq'
      [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
       LambdaSimple' ([],
        BoxSet' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Int 1)))))]])));;

_assert 2 "(lambda () x)" (LambdaSimple' ([], Var' (VarFree "x")));;

_assert 3 
"(lambda (x y) 
   (lambda () x) 
   (lambda () y)
   (lambda () (set! x y)))"
(LambdaSimple' (["x"; "y"],
 Seq'
  [Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
   Seq'
    [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
     LambdaSimple' ([], Var' (VarBound ("y", 0, 1)));
     LambdaSimple' ([],
      BoxSet' (VarBound ("x", 0, 0), Var' (VarBound ("y", 0, 1))))]]));;

_assert 4
"(lambda (z)
   (lambda () (lambda () (set! z (+ z 2))))
   (lambda () (lambda () (set! z (+ z 2)))))"
(LambdaSimple' (["z"],
  Seq'
   [Set' (Var' (VarParam ("z", 0)), Box' (VarParam ("z", 0)));
    Seq'
     [LambdaSimple' ([],
       LambdaSimple' ([],
        BoxSet' (VarBound ("z", 1, 0),
         Applic' (Var' (VarFree "+"),
          [BoxGet' (VarBound ("z", 1, 0)); Const' (Sexpr (Number (Int 2)))]))));
      LambdaSimple' ([],
       LambdaSimple' ([],
        BoxSet' (VarBound ("z", 1, 0),
         Applic' (Var' (VarFree "+"),
          [BoxGet' (VarBound ("z", 1, 0)); Const' (Sexpr (Number (Int 2)))]))))]]));;

_assert 5
"(lambda (a b c d)
  (lambda (a b c) (lambda () (lambda () (set! d (+ d 1)) (set! c (2)))))
  (lambda () c)
  (lambda () (set! d 1)))"
(LambdaSimple' (["a"; "b"; "c"; "d"],
 Seq'
  [Set' (Var' (VarParam ("d", 3)), Box' (VarParam ("d", 3)));
   Seq'
    [LambdaSimple' (["a"; "b"; "c"],
      LambdaSimple' ([],
       LambdaSimple' ([],
        Seq'
         [BoxSet' (VarBound ("d", 2, 3),
           Applic' (Var' (VarFree "+"),
            [BoxGet' (VarBound ("d", 2, 3)); Const' (Sexpr (Number (Int 1)))]));
          Set' (Var' (VarBound ("c", 1, 2)),
           Applic' (Const' (Sexpr (Number (Int 2))), []))])));
     LambdaSimple' ([], Var' (VarBound ("c", 0, 2)));
     LambdaSimple' ([],
      BoxSet' (VarBound ("d", 0, 3), Const' (Sexpr (Number (Int 1)))))]]));;

_assert 6
"(lambda ()
  (lambda ()
    (lambda (x y)
      (lambda ()
        (lambda () (set! x 4))
        (lambda () (set! x (lambda () x)) (set! y (lambda () y)))
      )
      (lambda () (lambda () (set! y 0)))
    )
  )
)"
(LambdaSimple' ([],
  LambdaSimple' ([],
   LambdaSimple' (["x"; "y"],
    Seq'
     [Set' (Var' (VarParam ("y", 1)), Box' (VarParam ("y", 1)));
      Seq'
       [LambdaSimple' ([],
         Seq'
          [LambdaSimple' ([],
            Set' (Var' (VarBound ("x", 1, 0)),
             Const' (Sexpr (Number (Int 4)))));
           LambdaSimple' ([],
            Seq'
             [Set' (Var' (VarBound ("x", 1, 0)),
               LambdaSimple' ([], Var' (VarBound ("x", 2, 0))));
              BoxSet' (VarBound ("y", 1, 1),
               LambdaSimple' ([], BoxGet' (VarBound ("y", 2, 1))))])]);
        LambdaSimple' ([],
         LambdaSimple' ([],
          BoxSet' (VarBound ("y", 1, 1), Const' (Sexpr (Number (Int 0))))))]]))));;

_assert 7
"(lambda ()
  (lambda ()
    (lambda (x y)
      (if (set! x (lambda () x y))
          (lambda () (set! y 4))
      )
    )
  )
)"
(LambdaSimple' ([],
  LambdaSimple' ([],
   LambdaSimple' (["x"; "y"],
    Seq'
     [Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
      Set' (Var' (VarParam ("y", 1)), Box' (VarParam ("y", 1)));
      If'
       (BoxSet' (VarParam ("x", 0),
         LambdaSimple' ([],
          Seq'
           [BoxGet' (VarBound ("x", 0, 0)); BoxGet' (VarBound ("y", 0, 1))])),
       LambdaSimple' ([],
        BoxSet' (VarBound ("y", 0, 1), Const' (Sexpr (Number (Int 4))))),
       Const' Void)]))));;

_assert 8
"(lambda (x y)
  (lambda ()
    (lambda ()
      (if (set! x 5)
          (set! y 4)
    )
    )
  )
  y
)"
(LambdaSimple' (["x"; "y"],
  Seq'
   [Set' (Var' (VarParam ("y", 1)), Box' (VarParam ("y", 1)));
    Seq'
     [LambdaSimple' ([],
       LambdaSimple' ([],
        If'
         (Set' (Var' (VarBound ("x", 1, 0)), Const' (Sexpr (Number (Int 5)))),
         BoxSet' (VarBound ("y", 1, 1), Const' (Sexpr (Number (Int 4)))),
         Const' Void)));
      BoxGet' (VarParam ("y", 1))]]));;

_assert 9
"(lambda ()
  (lambda (x y z)
    (lambda () (set! x y))
    (lambda () (set! y (+ x z)) (set! z 5))
  )
)"
(LambdaSimple' ([],
  LambdaSimple' (["x"; "y"; "z"],
   Seq'
    [Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
     Set' (Var' (VarParam ("y", 1)), Box' (VarParam ("y", 1)));
     Seq'
      [LambdaSimple' ([],
        BoxSet' (VarBound ("x", 0, 0), BoxGet' (VarBound ("y", 0, 1))));
       LambdaSimple' ([],
        Seq'
         [BoxSet' (VarBound ("y", 0, 1),
           Applic' (Var' (VarFree "+"),
            [BoxGet' (VarBound ("x", 0, 0)); Var' (VarBound ("z", 0, 2))]));
          Set' (Var' (VarBound ("z", 0, 2)), Const' (Sexpr (Number (Int 5))))])]])));;

_assert 10
"(lambda (a b . c)
  (lambda ()
    (set! c (+ c a))
    c
  )
  (lambda () (set! b (lambda () (set! a 5))))
)"
(LambdaOpt' (["a"; "b"], "c",
  Seq'
   [Set' (Var' (VarParam ("a", 0)), Box' (VarParam ("a", 0)));
    Seq'
     [LambdaSimple' ([],
       Seq'
        [Set' (Var' (VarBound ("c", 0, 2)),
          Applic' (Var' (VarFree "+"),
           [Var' (VarBound ("c", 0, 2)); BoxGet' (VarBound ("a", 0, 0))]));
         Var' (VarBound ("c", 0, 2))]);
      LambdaSimple' ([],
       Set' (Var' (VarBound ("b", 0, 1)),
        LambdaSimple' ([],
         BoxSet' (VarBound ("a", 1, 0), Const' (Sexpr (Number (Int 5)))))))]]));;

_assert 11
"(lambda (a b . c)
  (lambda ()
    (set! a (+ a a))
    c
  )
  (lambda () (set! b (lambda () (set! c 5))))
)"
(LambdaOpt' (["a"; "b"], "c",
  Seq'
   [Set' (Var' (VarParam ("c", 2)), Box' (VarParam ("c", 2)));
    Seq'
     [LambdaSimple' ([],
       Seq'
        [Set' (Var' (VarBound ("a", 0, 0)),
          Applic' (Var' (VarFree "+"),
           [Var' (VarBound ("a", 0, 0)); Var' (VarBound ("a", 0, 0))]));
         BoxGet' (VarBound ("c", 0, 2))]);
      LambdaSimple' ([],
       Set' (Var' (VarBound ("b", 0, 1)),
        LambdaSimple' ([],
         BoxSet' (VarBound ("c", 1, 2), Const' (Sexpr (Number (Int 5)))))))]]));;

_assert 12
"(begin
  (lambda (x)
    (or (lambda () (set! x (+ x 0)))
        (set! a (+ x 1))
    )
  )
  (lambda (x)
    (lambda ()
      (or (set! x (+ x 2))
          (set! x (+ x 3))  
      )
    )
  )
)"
(Seq'
  [LambdaSimple' (["x"],
    Seq'
     [Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
      Or'
       [LambdaSimple' ([],
         BoxSet' (VarBound ("x", 0, 0),
          Applic' (Var' (VarFree "+"),
           [BoxGet' (VarBound ("x", 0, 0)); Const' (Sexpr (Number (Int 0)))])));
        Set' (Var' (VarFree "a"),
         Applic' (Var' (VarFree "+"),
          [BoxGet' (VarParam ("x", 0)); Const' (Sexpr (Number (Int 1)))]))]]);
   LambdaSimple' (["x"],
    LambdaSimple' ([],
     Or'
      [Set' (Var' (VarBound ("x", 0, 0)),
        Applic' (Var' (VarFree "+"),
         [Var' (VarBound ("x", 0, 0)); Const' (Sexpr (Number (Int 2)))]));
       Set' (Var' (VarBound ("x", 0, 0)),
        Applic' (Var' (VarFree "+"),
         [Var' (VarBound ("x", 0, 0)); Const' (Sexpr (Number (Int 3)))]))]))]);;

_assert 13
"(begin
  (lambda (x)
    (lambda ()
      (set! x (set! x 4))
    )
    (lambda ()
      (set! x 5)
    )
  )
  (lambda (x)
    (or (set! x (+ x 2))
        (set! x (+ x 3))
        x
    )
  )
)"
(Seq'
  [LambdaSimple' (["x"],
    Seq'
     [LambdaSimple' ([],
       Set' (Var' (VarBound ("x", 0, 0)),
        Set' (Var' (VarBound ("x", 0, 0)), Const' (Sexpr (Number (Int 4))))));
      LambdaSimple' ([],
       Set' (Var' (VarBound ("x", 0, 0)), Const' (Sexpr (Number (Int 5)))))]);
   LambdaSimple' (["x"],
    Or'
     [Set' (Var' (VarParam ("x", 0)),
       Applic' (Var' (VarFree "+"),
        [Var' (VarParam ("x", 0)); Const' (Sexpr (Number (Int 2)))]));
      Set' (Var' (VarParam ("x", 0)),
       Applic' (Var' (VarFree "+"),
        [Var' (VarParam ("x", 0)); Const' (Sexpr (Number (Int 3)))]));
      Var' (VarParam ("x", 0))])]);;

_assert 14
"(lambda (x y z)
  (lambda (z x y)
    (lambda (y z x)
      (set! x x)
      (begin x x x)
    )
    (lambda () (set! x x))
    x
  )
  (lambda () (lambda () x))
  (set! x 0)
)"

(LambdaSimple' (["x"; "y"; "z"],
  Seq'
   [Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
    Seq'
     [LambdaSimple' (["z"; "x"; "y"],
       Seq'
        [Set' (Var' (VarParam ("x", 1)), Box' (VarParam ("x", 1)));
         Seq'
          [LambdaSimple' (["y"; "z"; "x"],
            Seq'
             [Set' (Var' (VarParam ("x", 2)), Var' (VarParam ("x", 2)));
              Seq'
               [Var' (VarParam ("x", 2)); Var' (VarParam ("x", 2));
                Var' (VarParam ("x", 2))]]);
           LambdaSimple' ([],
            BoxSet' (VarBound ("x", 0, 1), BoxGet' (VarBound ("x", 0, 1))));
           BoxGet' (VarParam ("x", 1))]]);
      LambdaSimple' ([], LambdaSimple' ([], BoxGet' (VarBound ("x", 1, 0))));
      BoxSet' (VarParam ("x", 0), Const' (Sexpr (Number (Int 0))))]]));;

_assert 15
"(lambda (x)
  (set! x
    (lambda ()
      (set! x
        (lambda()
          (set! x
            (lambda () x)
          ) 
        )
      )
    )
  )
)"

(LambdaSimple' (["x"],
  Seq'
   [Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
    BoxSet' (VarParam ("x", 0),
     LambdaSimple' ([],
      BoxSet' (VarBound ("x", 0, 0),
       LambdaSimple' ([],
        BoxSet' (VarBound ("x", 1, 0),
         LambdaSimple' ([], BoxGet' (VarBound ("x", 2, 0))))))))]));;
#use "tag-parser.ml";;
open Tag_Parser;;
open Reader;;

let fail_or_pass bool = 
  if bool then "passed" else "failed";;

let output_from_tag input = tag_parse_expression (read_sexpr input);;
let test_ input expected_output = 
  let output = output_from_tag input in
    print_string (String.concat " " ["\n\ntest input:"; 
    input; 
    "\ntest output:";
    fail_or_pass (expected_output=output)]);;

let test_sexprs_191 sexp_input expected_output test_num = 
  let output = tag_parse_expressions sexp_input in
    print_string (String.concat " " ["\n\ntest input:"; 
      test_num; 
      "\ntest output:";
    fail_or_pass (expected_output=output)]);;

let test_sexpr_191 sexp_input expected_output test_num = 
  let output = tag_parse_expression sexp_input in
    print_string (String.concat " " ["\n\ntest input:"; 
      test_num; 
      "\ntest output:";
    fail_or_pass (expected_output=output)]);;

let exc_checker_expr input = 
  try (output_from_tag input)
  with X_syntax_error -> Const(Void);;

let test_exception input = 
let output = exc_checker_expr input in
  print_string (String.concat " " ["\n\ntest input:"; 
  input; 
  "\ntest output:";
  fail_or_pass (Const(Void)=output)]);;                                      
                                    
let rec print_list = function 
  [] -> ()
  | e::l -> print_string e ; print_string " " ; print_list l;;
  
(* Const *)
  test_ "#{x}='()" (Const (Sexpr (TaggedSexpr ("x", Nil))));;
  test_ "#{x}='(a b)" (Const (Sexpr (TaggedSexpr ("x", Pair (Symbol "a", Pair (Symbol "b", Nil))))));;

(* Lambda *)
  test_ "(lambda (x) '(1 2 3) 5 6)" (LambdaSimple (["x"],
  Seq
  [Const
    (Sexpr
      (Pair (Number (Int 1), Pair (Number (Int 2), Pair (Number (Int 3), Nil)))));
    Const (Sexpr (Number (Int 5))); Const (Sexpr (Number (Int 6)))]));;
  test_ "(lambda (x y z) 8 \"asdasd!\")" (LambdaSimple (["x"; "y"; "z"],
  Seq
  [Const (Sexpr (Number (Int 8))); Const (Sexpr (String "asdasd!"))]));;
  test_ "(lambda x x)" (LambdaOpt ([], "x", Var "x"));;
  test_ "(lambda (x y z . vs) y z x vs)" (LambdaOpt (["x"; "y"; "z"], "vs", Seq [Var "y"; Var "z"; Var "x"; Var "vs"]));;
  test_ "(lambda (x y z . vs) y)" (LambdaOpt (["x"; "y"; "z"], "vs", Var "y"));;
  test_exception "(lambda (x y z . vs) )";;
  test_exception "(lambda (x x) (+ 1 2))";;
 

(* Or *)
  test_ "(or)" (Const (Sexpr (Bool false)));;
  test_ "(or 1 2)"  (Or [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)))]);; 
  test_ "(Or)" (Const (Sexpr (Bool false)));;
  test_ "(OR 1 2)"  (Or [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)))]);; 
  test_ "(or #t #f . (2))" (Or
  [Const (Sexpr (Bool true)); Const (Sexpr (Bool false));
  Const (Sexpr (Number (Int 2)))]);;
  test_ "(or #t #f . (2 . ()))" (Or
  [Const (Sexpr (Bool true)); Const (Sexpr (Bool false));
  Const (Sexpr (Number (Int 2)))]);;
  test_ "(Or 1 5 #t #f)" (Or
  [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 5)));
  Const (Sexpr (Bool true)); Const (Sexpr (Bool false))]);; 
  test_ "(or 1)" (Const (Sexpr (Number (Int 1))));;

  test_ "(Or 1 5 #t #f #\\t '(5 8 9))" (Or
  [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 5)));
    Const (Sexpr (Bool true)); Const (Sexpr (Bool false));
    Const (Sexpr (Char 't'));
    Const
    (Sexpr
      (Pair (Number (Int 5), Pair (Number (Int 8), Pair (Number (Int 9), Nil)))))]);;


(* Def *)
  test_ "(define a 5)" (Def (Var "a", Const (Sexpr (Number (Int 5)))));;
  test_ "(define a (or 5 6))" (Def (Var "a",
  Or [Const (Sexpr (Number (Int 5))); Const (Sexpr (Number (Int 6)))]));;
  test_ "(define a)" (Def (Var "a", Const Void));;
  test_ "(define HALOOOOOOO123 51321513212)" (Def (Var "halooooooo123", Const (Sexpr (Number (Int 51321513212)))));;
  test_ "(define HALOOOOOOO123 (lambda (x) x))" (Def (Var "halooooooo123", LambdaSimple (["x"], Var "x")));;
  test_ "(define NICELAMBDUSHHH (lambda (x y z) 8 \"asdasd!\"))" (Def (Var "nicelambdushhh",
  LambdaSimple (["x"; "y"; "z"],
   Seq
    [Const (Sexpr (Number (Int 8))); Const (Sexpr (String "asdasd!"))])));;

(* Set! *)
  test_ "(set! a 5)" (Set (Var "a", Const (Sexpr (Number (Int 5)))));;
  test_ "(set! a (or 5 6))" (Set (Var "a",
  Or [Const (Sexpr (Number (Int 5))); Const (Sexpr (Number (Int 6)))]));;
  test_exception "(set! a)";;
  test_ "(set! HALOOOOOOO123 51321513212)" (Set (Var "halooooooo123", Const (Sexpr (Number (Int 51321513212)))));;
  test_ "(set! HALOOOOOOO123 (lambda (x) x))" (Set (Var "halooooooo123", LambdaSimple (["x"], Var "x")));;
  test_ "(set! NICELAMBDUSHHH (lambda (x y z) 8 \"asdasd!\"))" (Set (Var "nicelambdushhh",
  LambdaSimple (["x"; "y"; "z"],
  Seq
    [Const (Sexpr (Number (Int 8))); Const (Sexpr (String "asdasd!"))])));;

(* Explicit Seq *)
  test_ "(begin)" (Const Void);;
  test_ "(begin a)" (Var "a");;
  test_ "(begin '(1 2 3))" (Const
  (Sexpr
    (Pair (Number (Int 1), Pair (Number (Int 2), Pair (Number (Int 3), Nil))))));;
  test_ "(begin (lambda (x y z) 8 \"asdasd!\"))" (LambdaSimple (["x"; "y"; "z"],
  Seq [Const (Sexpr (Number (Int 8))); Const (Sexpr (String "asdasd!"))]));;
  test_ "(begin (lambda (x y z) 8 \"asdasd!\") 5)" (Seq
  [LambdaSimple (["x"; "y"; "z"],
    Seq [Const (Sexpr (Number (Int 8))); Const (Sexpr (String "asdasd!"))]);
   Const (Sexpr (Number (Int 5)))]);;

   test_ "(lambda (x y z) 8 \"asdasd!\" (begin '(1 2) 5 6))" (LambdaSimple (["x"; "y"; "z"],
   Seq
    [Const (Sexpr (Number (Int 8))); Const (Sexpr (String "asdasd!"));
     Seq
      [Const (Sexpr (Pair (Number (Int 1), Pair (Number (Int 2), Nil))));
       Const (Sexpr (Number (Int 5))); Const (Sexpr (Number (Int 6)))]]));;


(* If *)
  test_ "(if 1 2 3)" (If (Const (Sexpr (Number (Int 1))), Const (Sexpr (Number (Int 2))),
  Const (Sexpr (Number (Int 3)))));;
  test_ "(if #t maybe)" (If (Const (Sexpr (Bool true)), Var "maybe", Const Void));;
  test_exception "(if #t)";;
  test_exception "(if #t asd asd haha)";;
  read_sexpr "(if #t 5r55r '())" ;;
  test_ "(if #t 5r55r '())" (If (Const (Sexpr (Bool true)), Var "5r55r", Const (Sexpr Nil)));;
  test_ "(if value ((f) value) 6)" (If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),
          Const (Sexpr (Number (Int 6)))));;
  test_ "(if value ((f) value) 6)" (If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),
      Const (Sexpr (Number (Int 6)))));;

(* Applic *)
  test_ "(+ 1 2)" (Applic (Var "+",
  [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)))]));;
  test_ "(- 1 2)" (Applic (Var "-",
  [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)))]));;
  test_ "(append '(1 2 . (3)) '(4))" (Applic (Var "append",
    [Const
      (Sexpr
        (Pair (Number (Int 1), Pair (Number (Int 2), Pair (Number (Int 3), Nil)))));
      Const (Sexpr (Pair (Number (Int 4), Nil)))]));;
  test_ "(fun1 '(1 2 . (3)) '(4))" (Applic (Var "fun1",
    [Const
      (Sexpr
        (Pair (Number (Int 1), Pair (Number (Int 2), Pair (Number (Int 3), Nil)))));
      Const (Sexpr (Pair (Number (Int 4), Nil)))]));;
  test_ "(append `(1 2 . (3)) '(4 . ()))" (Applic (Var "append",
 [Applic (Var "cons",
   [Const (Sexpr (Number (Int 1)));
    Applic (Var "cons",
     [Const (Sexpr (Number (Int 2)));
      Applic (Var "cons", [Const (Sexpr (Number (Int 3))); Const (Sexpr Nil)])])]);
  Const (Sexpr (Pair (Number (Int 4), Nil)))]));;


(* QQ *)
  test_ "`,5" (Const (Sexpr (Number (Int 5))));;
  test_ "`,x" (Var "x");;
  test_exception "`,@x";;
  test_ "`(a (1 2 3 . 5) (aasdasd asd ,@asd sa) ,@(asnd jksandjk nsadjk nsajkd naks) b c)" (Applic (Var "cons",
      [Const (Sexpr (Symbol "a"));
        Applic (Var "cons",
        [Applic (Var "cons",
      [Const (Sexpr (Number (Int 1)));
        Applic (Var "cons",
        [Const (Sexpr (Number (Int 2)));
          Applic (Var "cons",
          [Const (Sexpr (Number (Int 3))); Const (Sexpr (Number (Int 5)))])])]);
      Applic (Var "cons",
      [Applic (Var "cons",
        [Const (Sexpr (Symbol "aasdasd"));
          Applic (Var "cons",
          [Const (Sexpr (Symbol "asd"));
            Applic (Var "append",
            [Var "asd";
              Applic (Var "cons",
              [Const (Sexpr (Symbol "sa")); Const (Sexpr Nil)])])])]);
        Applic (Var "append",
        [Applic (Var "asnd",
          [Var "jksandjk"; Var "nsadjk"; Var "nsajkd"; Var "naks"]);
          Applic (Var "cons",
          [Const (Sexpr (Symbol "b"));
            Applic (Var "cons", [Const (Sexpr (Symbol "c")); Const (Sexpr Nil)])])])])])]));;

  test_ "`(,@a b)" (Applic (Var "append",
    [Var "a";
      Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])]));; 

  test_ "`(,@a b c)" (Applic (Var "append",
    [Var "a";
      Applic (Var "cons",
   [Const (Sexpr (Symbol "b"));
    Applic (Var "cons", [Const (Sexpr (Symbol "c")); Const (Sexpr Nil)])])]));;
  
  test_ "`(,@a ,@b c)" (Applic (Var "append",
    [Var "a";
      Applic (Var "append",
   [Var "b";
    Applic (Var "cons", [Const (Sexpr (Symbol "c")); Const (Sexpr Nil)])])]));;

  test_ "`(a b)" (Applic (Var "cons",
      [Const (Sexpr (Symbol "a"));
        Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])]));;

  test_ "`(a ,b)" (Applic (Var "cons",
      [Const (Sexpr (Symbol "a"));
        Applic (Var "cons", [Var "b"; Const (Sexpr Nil)])]));;

  test_ "`(,a ,@b)" (Applic (Var "cons",
    [Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])]));;

  test_ "`(,@a ,@b)" (Applic (Var "append",
    [Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])]));;

  test_ "`(,@a . ,b)" (Applic (Var "append", [Var "a"; Var "b"]));;

  test_ "`(,a . ,@b)" (Applic (Var "cons", [Var "a"; Var "b"]));;

  test_ "`(((,@a)))" (Applic (Var "cons",
      [Applic (Var "cons",
        [Applic (Var "append", [Var "a"; Const (Sexpr Nil)]); Const (Sexpr Nil)]);
        Const (Sexpr Nil)]));;

(* let *)
  test_ "(let ((x 2) (y 5)) (+ x y))" (Applic (LambdaSimple (["x"; "y"], Applic (Var "+", [Var "x"; Var "y"])),
    [Const (Sexpr (Number (Int 2))); Const (Sexpr (Number (Int 5)))]));;

  test_ "(let ((value (h? x)) (f (lambda () (p q)))) (f 5))" (Applic
        (LambdaSimple (["value"; "f"],
          Applic (Var "f", [Const (Sexpr (Number (Int 5)))])),
        [Applic (Var "h?", [Var "x"]); LambdaSimple ([], Applic (Var "p", [Var "q"]))]));;

(* Cond *)
  test_ "(cond ((zero? n) (f x) (g y)))" (If (Applic (Var "zero?", [Var "n"]),
    Seq [Applic (Var "f", [Var "x"]); Applic (Var "g", [Var "y"])], Const Void));;

  test_ "(cond (#t (f x) (g y)))" (If (Const (Sexpr (Bool true)),
    Seq [Applic (Var "f", [Var "x"]); Applic (Var "g", [Var "y"])], Const Void));;

  test_ "(cond (#t (f x)))" (If (Const (Sexpr (Bool true)), Applic (Var "f", [Var "x"]), Const Void));;

  test_ "(cond (#t ))" (If (Const (Sexpr (Bool true)), Const Void, Const Void));; 

  test_ "(cond ((zero? n) (f x) (g y))
                    (else (h x y) (g x))
                    ((q? y) (p x) (q y)))"  (If (Applic (Var "zero?", [Var "n"]),
              Seq [Applic (Var "f", [Var "x"]); Applic (Var "g", [Var "y"])],
              Seq [Applic (Var "h", [Var "x"; Var "y"]); Applic (Var "g", [Var "x"])]));;

  test_ "(cond ((h? x) => (p q)))" (Applic
          (LambdaSimple (["value"; "f"],
            If (Var "value", Applic (Applic (Var "f", []), [Var "value"]), Const Void)),
          [Applic (Var "h?", [Var "x"]); LambdaSimple ([], Applic (Var "p", [Var "q"]))]));;

  test_ "(cond ((zero? n) (f x) (g y))
              ((h? x) => (p q))
              (else (h x y) (g x))
              ((q? y) (p x) (q y)))"   
    (If (Applic (Var "zero?", [Var "n"]),
    Seq [Applic (Var "f", [Var "x"]); Applic (Var "g", [Var "y"])],
    Applic
      (LambdaSimple (["value"; "f"; "rest"],
        If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),
        Applic (Var "rest", []))),
      [Applic (Var "h?", [Var "x"]);
      LambdaSimple ([], Applic (Var "p", [Var "q"]));
      LambdaSimple ([],
        Seq [Applic (Var "h", [Var "x"; Var "y"]); Applic (Var "g", [Var "x"])])])));;

  test_ "(cond (x (begin x)) ((lambda (x) x) => (cond (else (lambda (x) (x))))) (else value))"
        (If (Var "x", Var "x", Applic (LambdaSimple (["value";"f";"rest"], If (Var "value", Applic (Applic (Var "f", []), 
        [Var "value"]), Applic (Var "rest", []))), [LambdaSimple (["x"], Var "x");LambdaSimple ([], LambdaSimple (["x"], 
        Applic (Var "x", [])));LambdaSimple ([], Var "value")])));;

(* and *)
  test_ "(and)" (Const (Sexpr (Bool true)));;

  test_ "(and a s b d s)" (If (Var "a",
              If (Var "s",
                If (Var "b", If (Var "d", Var "s", Const (Sexpr (Bool false))),
                Const (Sexpr (Bool false))),
                Const (Sexpr (Bool false))),
              Const (Sexpr (Bool false))));;

  test_ "(and #t)" (Const (Sexpr (Bool true)));;
  test_ "(and #f)" (Const (Sexpr (Bool false)));;
  test_ "(and 123)" (Const (Sexpr (Number (Int 123))));;
  test_ "(and asd 2e (define HALOOOOOOO123 51321513212) 234r 32 r32rnkjads fjksdfjku32 rjk4nr)"
      (If (Var "asd",
            If (Var "2e",
              If (Def (Var "halooooooo123", Const (Sexpr (Number (Int 51321513212)))),
              If (Var "234r",
                If (Const (Sexpr (Number (Int 32))),
                If (Var "r32rnkjads",
                  If (Var "fjksdfjku32", Var "rjk4nr", Const (Sexpr (Bool false))),
                  Const (Sexpr (Bool false))),
                Const (Sexpr (Bool false))),
                Const (Sexpr (Bool false))),
              Const (Sexpr (Bool false))),
              Const (Sexpr (Bool false))),
            Const (Sexpr (Bool false))));;

  test_ "(and asd 2e (or 1 2) (and #t ok))" (If (Var "asd",
              If (Var "2e",
                If (Or [Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)))],
                If (Const (Sexpr (Bool true)), Var "ok", Const (Sexpr (Bool false))),
                Const (Sexpr (Bool false))),
                Const (Sexpr (Bool false))),
              Const (Sexpr (Bool false))));;
    
(* let* & letrec *)
  test_ "(let* (a 3) (b 5) (+ a b))" (Applic
                  (LambdaSimple ([],
                    Applic
                      (LambdaSimple ([],
                        Seq
                        [Applic (Var "b", [Const (Sexpr (Number (Int 5)))]);
                          Applic (Var "+", [Var "a"; Var "b"])]),
                      [])),
                  []));;

  test_ "(let* ((a 2) (b (+ 1 a))) b)"   
                      (Applic
                        (LambdaSimple (["a"],
                          Applic (LambdaSimple (["b"], Var "b"),
                            [Applic (Var "+", [Const (Sexpr (Number (Int 1))); Var "a"])])),
                        [Const (Sexpr (Number (Int 2)))]));;


(* tests from 191 *)
  (* t1 *)
    test_sexprs_191 [Number (Int (1))] [Const (Sexpr (Number (Int (1))))] "t1";;
  (* t2 *)
    test_sexprs_191 [Pair (Symbol "quote", Pair (Nil, Nil))] [Const (Sexpr (Nil))] "t2";;
  (* t3 *)
    test_sexprs_191 [Pair (Symbol "quote", Pair (Pair (String "strin", Pair (Char 'g', Nil)), Nil))] [Const (Sexpr (Pair (String "strin", Pair (Char 'g', Nil))))] "t3";;
  (* t4 *)
    test_sexprs_191 [Symbol "1x"] [Var "1x"] "t4";;
  (* t6 *)
    test_sexprs_191 [Pair (Symbol "lambda", Pair (Nil, Pair (Pair (Symbol "quote", Pair (Pair (Number (Int (1)), Pair (Number (Int (2)), Nil)), Nil)), Pair (Number (Int (2)), Nil))))]
     [LambdaSimple ([], Seq ([Const (Sexpr (Pair (Number (Int (1)), Pair (Number (Int (2)), Nil))));Const (Sexpr (Number (Int (2))))]))] "t6";;
  (* t7 *)
    test_sexprs_191 [Pair (Symbol "define", Pair (Symbol "x", Pair (Number (Int (1)), Nil)))] [Def (Var "x", Const (Sexpr (Number (Int (1)))))] "t7";;
  (* t8 *)
    test_sexprs_191 [Pair (Symbol "define", Pair (Symbol "123x", Pair (Number (Int (123)), Nil)))] [Def (Var "123x", Const (Sexpr (Number (Int (123)))))] "t8";;
  (* t9 *)
    test_sexprs_191 [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair (Pair (Symbol "if", Pair (Symbol "x", Pair (Symbol "x", Pair (Bool false, Nil)))), Nil)))] 
    [LambdaSimple (["x"], If (Var "x", Var "x", Const (Sexpr (Bool false))))] "t9";;
  (* t10 *)
    test_sexprs_191 [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Pair (Symbol "if", Pair (Symbol "x", Pair (Symbol "x", Nil))), Nil)), Pair (Pair (Symbol "y", Pair (Pair (Symbol "void", Nil), Nil)), Nil)), Pair (Pair (Symbol "eq?", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil)))]
      [Applic (LambdaSimple (["x";"y"], Applic (Var "eq?", [Var "x";Var "y"])), [If (Var "x", Var "x", Const (Void));Applic (Var "void", [])])] "t10";;
  (* t11 *)
    test_sexpr_191 (Pair(Symbol "let", Pair(Nil, Pair(Pair(Symbol "letrec", Pair(Pair(Pair(Symbol "loop", Pair(Pair(Symbol "lambda", Pair(Pair(Symbol "x", Nil), Pair(Pair(Symbol "if", Pair(Pair(Symbol "null?", Pair(Symbol "x", Nil)), Pair(Pair(Symbol "quote", Pair(Symbol "done", Nil)), Pair(Pair(Symbol "loop", Pair(Pair(Symbol "cdr", Pair(Symbol "x", Nil)), Nil)), Nil)))), Nil))), Nil)), Nil), Pair(Pair(Symbol "loop", Pair(Pair(Symbol "quasiquote", Pair(Pair(Pair(Symbol "unquote", Pair(Symbol "a", Nil)), Pair(Pair(Symbol "unquote", Pair(Symbol "b", Nil)), Pair(Pair(Symbol "unquote", Pair(Symbol "c", Nil)), Nil))), Nil)), Nil)), Nil))), Nil))))
          (Applic (LambdaSimple ([], Applic (LambdaSimple (["loop"], Seq ([Set (Var "loop", LambdaSimple (["x"], If (Applic (Var "null?", [Var "x"]), Const (Sexpr (Symbol "done")), Applic (Var "loop", [Applic (Var "cdr", [Var "x"])]))));Applic (Var "loop", [Applic (Var "cons", [Var "a";Applic (Var "cons", [Var "b";Applic (Var "cons", [Var "c";Const (Sexpr (Nil))])])])])])), [Const (Sexpr (Symbol "whatever"))])), []))
          "t11";;
  (* t12 *)
    test_sexprs_191 [Pair (Pair (Symbol "or", Pair (Pair (Symbol "and", Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Symbol "<test>", Nil))), Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Symbol "<then>", Nil))), Nil))), Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Symbol "<else>", Nil))), Nil))), Nil)] 
     [Applic (Or ([If (LambdaSimple ([], Var "<test>"), LambdaSimple ([], Var "<then>"), Const (Sexpr (Bool false)));LambdaSimple ([], Var "<else>")]), [])] 
     "t12";;
  (* t13 *)
    test_sexprs_191 [Pair (Symbol "let*", Pair (Pair (Pair (Symbol "x", Pair (Pair (Symbol "quasiquote", Pair (Pair (Symbol "a", Pair (Symbol "=", Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)), Nil))), Nil)), Nil)), Pair (Pair (Symbol "y", Pair (Pair (Symbol "car", Pair (Symbol "x", Nil)), Nil)), Nil)), Pair (Pair (Symbol "quasiquote", Pair (Pair (Symbol "y", Pair (Pair (Symbol "unquote", Pair (Pair (Symbol "cdr", Pair (Symbol "x", Nil)), Nil)), Nil)), Nil)), Nil)))] 
    [Applic (LambdaSimple (["x"], Applic (LambdaSimple (["y"], Applic (Var "cons", [Const (Sexpr (Symbol "y"));Applic (Var "cons", [Applic (Var "cdr", [Var "x"]);Const (Sexpr (Nil))])])), [Applic (Var "car", [Var "x"])])), [Applic (Var "cons", [Const (Sexpr (Symbol "a"));Applic (Var "cons", [Const (Sexpr (Symbol "="));Applic (Var "append", [Var "a";Const (Sexpr (Nil))])])])])] 
    "t13";;
  (* t14 *)
    test_sexprs_191 [Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair (Pair (Symbol "let", Pair (Nil, Pair (Pair (Symbol "set!", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil))), Nil)))] 
    [LambdaSimple (["x"], Applic (LambdaSimple ([], Set (Var "x", Var "y")), []))] 
    "t14";;
  (* t15 *)
    test_sexprs_191 
    [Pair (Symbol "define", Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "z")), Pair (Pair (Symbol "eq?", Pair (String "y", Pair (Symbol "y", Nil))), Nil)))]
    [Def (Var "x", LambdaOpt (["y"], "z", Applic (Var "eq?", [Const (Sexpr (String "y"));Var "y"])))]
     "t15";;
  (* t16 *)
    test_sexprs_191 
    [Pair (Symbol "define", Pair (Pair (Symbol "x", Symbol "z"), Pair (Pair (Symbol "eq?", Pair (Char 'y', Pair (Pair (Symbol "car", Pair (Symbol "z", Nil)), Nil))), Nil)))]
    [Def (Var "x", LambdaOpt ([], "z", Applic (Var "eq?", [Const (Sexpr (Char 'y'));Applic (Var "car", [Var "z"])])))]
     "t16";;
  (* t17 *)
    test_sexprs_191 
    [Pair (Symbol "define", Pair (Pair (Symbol "x", Symbol "y"), Pair (Pair (Symbol "eq?", Pair (Pair (Symbol "quote", Pair (Nil, Nil)), Pair (Symbol "y", Nil))), Nil)))]
      [Def (Var "x", LambdaOpt ([], "y", Applic (Var "eq?", [Const (Sexpr (Nil));Var "y"])))]
     "t17";;
  (* t18 *)
    test_sexprs_191
    [Pair (Symbol "letrec", Pair (Pair (Pair (Symbol "loop", Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair (Pair (Symbol "if", Pair (Symbol "x", Pair (Pair (Symbol "set!", Pair (Symbol "y", Pair (Bool true, Nil))), Pair (Pair (Symbol "loop", Pair (Symbol "y", Nil)), Nil)))), Nil))), Nil)), Nil), Pair (Pair (Symbol "loop", Pair (Symbol "x", Nil)), Nil)))]
    [Applic (LambdaSimple (["loop"], Seq ([Set (Var "loop", LambdaSimple (["x"], If (Var "x", Set (Var "y", Const (Sexpr (Bool true))), Applic (Var "loop", [Var "y"]))));Applic (Var "loop", [Var "x"])])), [Const (Sexpr (Symbol "whatever"))])]
    "t18";;
  (* t19 *)
    test_sexprs_191 
    [Pair (Symbol "cond", Pair (Pair (Bool true, Pair (Number (Int (1)), Pair (Number (Int (2)), Pair (Number (Int (3)), Nil)))), Nil))]
      [If (Const (Sexpr (Bool true)), Seq ([Const (Sexpr (Number (Int (1))));Const (Sexpr (Number (Int (2))));Const (Sexpr (Number (Int (3))))]), Const (Void))]
     "t19";;
  (* t20 *)
    test_sexprs_191 
    [Pair (Symbol "let*", Pair (Pair (Pair (Symbol "x", Pair (Number (Int (1)), Nil)), Pair (Pair (Symbol "y", Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Symbol "x", Nil))), Nil)), Nil)), Pair (Symbol "x", Pair (Pair (Symbol "y", Nil), Nil))))]
      [Applic (LambdaSimple (["x"], Applic (LambdaSimple (["y"], Seq [Var "x"; Applic (Var "y", [])]), [LambdaSimple ([], Var "x")])), [Const (Sexpr (Number (Int (1))))])]
     "t20";;
  (* t21 *)
    test_sexprs_191 
    [Pair (Symbol "if", Pair (Pair (Symbol "if", Pair (Symbol "a", Pair (Symbol "b", Pair (Char 'c', Nil)))), Pair (Pair (Symbol "quote", Pair (Symbol "x", Nil)), Pair (Pair (Symbol "quote", Pair (Pair (Symbol "+", Pair (Symbol "y", Pair (Symbol "z", Nil))), Nil)), Nil))))]
      [If (If (Var "a", Var "b", Const (Sexpr (Char 'c'))), Const (Sexpr (Symbol "x")), Const (Sexpr (Pair (Symbol "+", Pair (Symbol "y", Pair (Symbol "z", Nil))))))]
     "t21";;
  (* t22 *)
    test_sexprs_191 
    [Pair (Symbol "or", Pair (Pair (Symbol "zero?", Pair (Symbol "x", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "y", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "z", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "w", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "v", Nil)), Nil))))))]
      [Or ([Applic (Var "zero?", [Var "x"]);Applic (Var "zero?", [Var "y"]);Applic (Var "zero?", [Var "z"]);Applic (Var "zero?", [Var "w"]);Applic (Var "zero?", [Var "v"])])]
     "t22";;
  (* t23 *)
    test_sexprs_191 
    [Pair (Symbol "and", Pair (Pair (Symbol "zero?", Pair (Symbol "x", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "y", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "z", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "w", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "v", Nil)), Nil))))))]
      [If (Applic (Var "zero?", [Var "x"]), If (Applic (Var "zero?", [Var "y"]), If (Applic (Var "zero?", [Var "z"]), If (Applic (Var "zero?", [Var "w"]), Applic (Var "zero?", [Var "v"]), Const (Sexpr (Bool false))), Const (Sexpr (Bool false))), Const (Sexpr (Bool false))), Const (Sexpr (Bool false)))]
     "t23";;
  (* t24 *)
    test_sexprs_191 
    [Pair (Symbol "if", Pair (Pair (Symbol "begin", Pair (Number (Int (1)), Pair (Number (Int (2)), Pair (Number (Int (3)), Pair (Bool false, Nil))))), Pair (Number (Int (1)), Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Pair (Symbol "begin", Nil), Pair (Number (Int (1)), Pair (Number (Int (2)), Pair (Number (Int (3)), Pair (Bool false, Nil))))))), Nil))))]
      [If (Seq ([Const (Sexpr (Number (Int (1))));Const (Sexpr (Number (Int (2))));Const (Sexpr (Number (Int (3))));Const (Sexpr (Bool false))]), Const (Sexpr (Number (Int (1)))), LambdaSimple ([], Seq ([Const (Void);Const (Sexpr (Number (Int (1))));Const (Sexpr (Number (Int (2))));Const (Sexpr (Number (Int (3))));Const (Sexpr (Bool false))])))]
     "t24";;
  (* t26 *)
    test_sexprs_191 
    [Pair (Symbol "define", Pair (Pair (Symbol "foo", Pair (Symbol "x", Pair (Symbol "y", Nil))), Pair (Pair (Symbol "lambda", Pair (Symbol "y", Pair (Symbol "y", Pair (Symbol "y", Nil)))), Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "z", Symbol "w"), Pair (Symbol "z", Pair (Symbol "w", Nil)))), Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "z", Nil), Pair (Pair (Symbol "lambda", Pair (Symbol "w", Pair (Symbol "w", Nil))), Nil))), Nil)))))]
      [Def (Var "foo", LambdaSimple (["x";"y"], Seq ([LambdaOpt ([], "y", Seq ([Var "y";Var "y"]));LambdaOpt (["z"], "w", Seq ([Var "z";Var "w"]));LambdaSimple (["z"], LambdaOpt ([], "w", Var "w"))])))]
     "t26";;
  (* t27 *)
    test_sexprs_191 
    [Pair (Symbol "quasiquote", Pair (Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)), Pair (Symbol "b", Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "c", Nil)), Nil))), Nil))]
      [Applic (Var "cons", [Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b"));Applic (Var "append", [Var "c";Const (Sexpr (Nil))])])])]
     "t28";;
  (* 28 *)
    test_sexprs_191 
    [Pair (Pair (Symbol "quasiquote", Pair (Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "x", Nil)), Pair (Pair (Symbol "unquote", Pair (Pair (Symbol "set!", Pair (Symbol "y", Pair (Pair (Symbol "car", Pair (Symbol "x", Nil)), Nil))), Nil)), Nil)), Nil)), Nil)]
    [Applic (Applic (Var "append", [Var "x";Applic (Var "cons", [Set (Var "y", Applic (Var "car", [Var "x"]));Const (Sexpr (Nil))])]), [])] 
     "t28";;
  (* 29 *)
    test_sexprs_191 
    [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Int (1)), Nil)), Nil), Pair (Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Int (2)), Nil)), Nil), Pair (Symbol "x", Nil))), Nil))), Nil)), Nil), Pair (Pair (Symbol "set!", Pair (Symbol "x", Pair (Pair (Symbol "quote", Pair (Symbol "let", Nil)), Nil))), Nil)))]
      [Applic (LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Symbol "let")))), [Applic (LambdaSimple (["x"], Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Number (Int (2))))])), [Const (Sexpr (Number (Int (1))))])])]
     "t29";;
  (* t30 *)
    test_sexprs_191 
    [Pair (Symbol "cond", Pair (Pair (Symbol "x", Pair (Pair (Symbol "begin", Pair (Symbol "x", Nil)), Nil)), Pair (Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair (Symbol "x", Nil))), Pair (Symbol "=>", Pair (Pair (Symbol "cond", Pair (Pair (Symbol "else", Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair (Pair (Symbol "x", Nil), Nil))), Nil)), Nil)), Nil))), Pair (Pair (Symbol "else", Pair (Symbol "value", Nil)), Nil))))]
      [If (Var "x", Var "x", Applic (LambdaSimple (["value";"f";"rest"], If (Var "value", Applic (Applic (Var "f", []), [Var "value"]), Applic (Var "rest", []))), [LambdaSimple (["x"], Var "x");LambdaSimple ([], LambdaSimple (["x"], Applic (Var "x", [])));LambdaSimple ([], Var "value")]))]
     "t30";;
  (* t31 *)
    test_sexprs_191 
    [Pair (Symbol "or", Pair (Pair (Symbol "begin", Nil), Pair (Pair (Symbol "and", Nil), Nil)))]
      [Or ([Const (Void);Const (Sexpr (Bool true))])]
     "t31";;
  (* t32 *)
    test_sexprs_191 
    [Pair (Symbol "+", Pair (Pair (Symbol "and", Pair (Number (Int (1)), Nil)), Pair (Pair (Symbol "or", Pair (Number (Int (2)), Nil)), Nil)))]
      [Applic (Var "+", [Const (Sexpr (Number (Int (1))));Const (Sexpr (Number (Int (2))))])]
     "t32";;
  (* t33 *)
    test_sexprs_191 
    [Pair (Symbol "quote", Pair (Pair (Symbol "cond", Pair (Pair (Symbol "let", Nil), Pair (Pair (Symbol "or", Pair (Symbol "and", Nil)), Nil))), Nil))]
      [Const (Sexpr (Pair (Symbol "cond", Pair (Pair (Symbol "let", Nil), Pair (Pair (Symbol "or", Pair (Symbol "and", Nil)), Nil)))))]
     "t33";;
  (* t34 *)
    test_sexprs_191 
    [Pair (Symbol "begin", Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Pair (Symbol "begin", Pair (Pair (Symbol "quote", Pair (Symbol "a", Nil)), Pair (Pair (Symbol "quote", Pair (Symbol "b", Nil)), Nil))), Pair (Pair (Symbol "quote", Pair (Symbol "c", Nil)), Nil)))), Pair (Pair (Symbol "begin", Pair (Pair (Symbol "quote", Pair (Symbol "d", Nil)), Nil)), Nil)))]
      [Seq ([LambdaSimple ([], Seq ([Seq ([Const (Sexpr (Symbol "a"));Const (Sexpr (Symbol "b"))]);Const (Sexpr (Symbol "c"))]));Const (Sexpr (Symbol "d"))])]
     "t34";;
  (* t35 *)
    test_sexprs_191 
    [Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "c", Nil), Pair (Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "a", Nil), Pair (Symbol "a", Pair (Pair (Symbol "quote", Pair (Symbol "b", Nil)), Nil)))), Pair (Symbol "c", Nil)), Nil))), Pair (Pair (Symbol "begin", Pair (Pair (Symbol "quote", Pair (Symbol "d", Nil)), Nil)), Nil))]
      [Applic (LambdaSimple (["c"], Applic (LambdaSimple (["a"], Seq ([Var "a";Const (Sexpr (Symbol "b"))])), [Var "c"])), [Const (Sexpr (Symbol "d"))])]
     "t35";;
  (* t36 *)
    test_sexprs_191 
    [Number (Int 1); Number (Int 2); Symbol "x"; Symbol "y"; Symbol "z"]
      [Const (Sexpr (Number (Int (1))));Const (Sexpr (Number (Int (2))));Var "x";Var "y";Var "z"]
     "t36";;
  (* t37 *)
    test_sexprs_191 
    [Pair (Symbol "define",
  Pair (Pair (Symbol "foo", Pair (Symbol "x", Nil)),
   Pair (Pair (Symbol "zero?", Pair (Symbol "x", Nil)), Nil)));
 Pair (Symbol "cond",
  Pair
   (Pair (Bool false, Pair (Symbol "=>", Pair (Symbol "foo", Nil))),
   Nil))]

      [Def (Var "foo", LambdaSimple (["x"], Applic (Var "zero?", [Var "x"])));Applic (LambdaSimple (["value";"f"], If (Var "value", Applic (Applic (Var "f", []), [Var "value"]), Const (Void))), [Const (Sexpr (Bool false));LambdaSimple ([], Var "foo")])]
     "t37";;

(* DEFINE MIT *)
  test_ "(define (xx . ab) . (+ 1 2))" (Def (Var "xx",
                    LambdaOpt ([], "ab",
                      Seq [Var "+"; Const (Sexpr (Number (Int 1))); Const (Sexpr (Number (Int 2)))])));;


  test_ "#{x}" (Const(Sexpr (TagRef ("x"))));;


 (* output_from_tag "(lambda (x x) (+ 1 2))";; *)
(* output_from_tag *)
(* read_sexpr *)
(* test_ *)  

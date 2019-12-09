#use "tag-parser.ml"
open Tag_Parser
(**********TESTING**********)

let _tag_string str =
  let sexp = (read_sexpr str) in
  tag_parse_expression sexp;;

exception X_test_mismatch;;

(*Test will fail if no X_syntax_error is raised with input str*)
let _assertX num str =
  try let sexpr = (tag_parse_expression (read_sexpr str)) in
      match sexpr with
      |_ ->
        (failwith
	(Printf.sprintf
	   "Failed %.1f: Expected syntax error with string '%s'"num str))
   with
  |X_no_match ->
     (failwith
	(Printf.sprintf
	   "Failed %.1f with X_no_match: Reader couldn't parse the string '%s'"num str))
  |X_syntax_error -> num

(*Test will fail if an exception is raised,
or the output of parsing str is different than the expression out*)
let _assert num str out =
  try let sexpr = (read_sexpr str) in
      (if not (expr_eq (tag_parse_expression sexpr) out)
       then raise X_test_mismatch
       else num)
  with
  |X_no_match ->
     (failwith
	(Printf.sprintf
	   "Failed %.2f with X_no_match: Reader couldn't parse the string '%s'"num str))
  |X_test_mismatch ->
    (failwith
       (Printf.sprintf
	  "Failed %.2f with mismatch: The input -- %s -- produced unexpected expression"num str))
  |X_syntax_error ->
     (failwith
	(Printf.sprintf
	   "Failed %.2f with X_syntax_error: Tag parser failed to resolve expression '%s'"num str));;

(*Boolean*)
_assert 1.0 "#t" ( Const (Sexpr (Bool true)));;
_assert 1.1 "#f" ( Const (Sexpr (Bool false)));;

(*Number*)
_assert 2.0 "123" ( Const (Sexpr (Number (Int 123))));;
_assert 2.1 "-123" ( Const (Sexpr (Number (Int (-123)))));;
_assert 2.2 "12.3" ( Const (Sexpr (Number (Float (12.3)))));;
_assert 2.3 "-12.3" ( Const (Sexpr (Number (Float (-12.3)))));;


(*Char*)
_assert 3.0 "#\\A" ( Const (Sexpr (Char 'A')));;
_assert 3.1 "#\\nul" ( Const (Sexpr (Char '\000')));;


(*String*)
_assert 4.0 "\"String\"" (Const (Sexpr (String "String")));;


(*Quote*)
_assert 5.0 "'quoting" (Const (Sexpr (Symbol "quoting")));;
(*_assert 5.1 ",unquoting" (Const (Sexpr (Symbol "unquoting")));; removed - invalid syntax*)

(*Symbol*)
_assert 6.0 "symbol" (Var "symbol");;

(*If*)
_assert 7.0 "(if #t 2 \"abc\")"
  (If (Const (Sexpr (Bool true)), Const (Sexpr (Number (Int 2))),
       Const (Sexpr (String "abc"))));;

_assert 7.1 "(if #t 2)"
  (If (Const (Sexpr (Bool true)), Const (Sexpr (Number (Int 2))),
       (Const Void)));;

(*SimpleLambda*)
_assert 8.0 "(lambda (a b c) d)" (LambdaSimple (["a"; "b"; "c"], Var "d"));;
_assert 8.1 "(lambda (a b c) (begin d))" (LambdaSimple (["a"; "b"; "c"], Var "d"));;
_assert 8.2 "(lambda (a b c) a b)" (LambdaSimple (["a"; "b"; "c"], Seq [Var "a"; Var "b"]));;
_assert 8.3 "(lambda (a b c) (begin a b))" (LambdaSimple (["a"; "b"; "c"], Seq [Var "a"; Var "b"]));;
_assert 8.4 "(lambda (a b c) (begin))" (LambdaSimple (["a"; "b"; "c"], Const Void));;
_assertX 8.5 "(lambda (a b c d d) e f)";;
_assert 8.6 "(lambda () e f)" (LambdaSimple( [], Seq [Var "e"; Var "f"])) ;;

(*LambdaOpt*)
_assert 9.0 "(lambda (a b . c) d)" ( LambdaOpt (["a"; "b"], "c", Var "d"));;
_assert 9.1 "(lambda (a b . c) (begin d))" ( LambdaOpt (["a"; "b"], "c", Var "d"));;
_assert 9.2 "(lambda (a b . c) d e)" ( LambdaOpt (["a"; "b"], "c",  Seq [Var "d"; Var "e"]));;
_assert 9.3 "(lambda (a b . c) (begin d e))" ( LambdaOpt (["a"; "b"], "c",  Seq [Var "d"; Var "e"]));;
_assert 9.4 "(lambda (a b . c) (begin) )" ( LambdaOpt (["a"; "b"], "c",  Const Void));;
_assertX 9.5 "(lambda (a b c d .a) e f)";;



(*Lambda Variadic*)
_assert 10.0 "(lambda a d)" ( LambdaOpt ([], "a", Var "d"));;
_assert 10.1 "(lambda a (begin d))" ( LambdaOpt ([], "a", Var "d"));;
_assert 10.2 "(lambda a d e)" ( LambdaOpt ([], "a", Seq [Var "d"; Var "e"] ));;
_assert 10.3 "(lambda a (begin d e))" ( LambdaOpt ([], "a",  Seq [Var "d"; Var "e"]));;
_assert 10.4 "(lambda a (begin) )" ( LambdaOpt ([], "a",  Const Void));;

(*Application*)
_assert 11.0 "(+ 1 2 3)"
  (Applic (Var "+", [Const (Sexpr (Number (Int 1)));
		     Const (Sexpr (Number (Int 2)));
		     Const (Sexpr (Number (Int 3)))]));;
_assert 11.1 "((lambda (v1 v2) c1 c2 c3) b1 b2)"
  (Applic
     (LambdaSimple (["v1"; "v2"],
		    Seq [Var "c1"; Var "c2"; Var "c3"]),
      [Var "b1"; Var "b2"]));;

(*Or*)
_assert 12.0 "(or #t #f #\\a)"
  (Or
     [Const (Sexpr (Bool true)); Const (Sexpr (Bool false));
      Const (Sexpr (Char 'a'))]);;

_assert 12.1 "(or 'a)"  (Or [Const (Sexpr (Symbol "a"))]);;

(* based on forum answers, the case with one expression is only *evaluated* to that expression,
but its still parsed as an Or expression at this point
(Const (Sexpr (Symbol "a")));;*)

_assert 12.2 "(or)"
  (Const (Sexpr (Bool false)));;

(*Define*)
_assert 13.0 "(define a b)" (Def (Var "a", Var "b"));;
_assertX 13.1 "(define 5 b)";;
_assertX 13.2 "(define if b)";;

(*Set*)
_assert 14.0 "(set! a 5)" (Set (Var "a", Const (Sexpr (Number (Int 5)))));;
_assertX 14.1 "(set! define 5)";;
_assertX 14.2 "(set! \"string\" 5)";;


(*Let*)
_assert 15.0 "(let ((v1 b1)(v2 b2)) c1 c2 c3)"
  (Applic (LambdaSimple (["v1"; "v2"], Seq [Var "c1"; Var "c2"; Var "c3"]), [Var "b1"; Var "b2"]));;
_assert 15.1 "(let () c1 c2)" (Applic (LambdaSimple ([], Seq [Var "c1"; Var "c2"]), []));;

(*And*)
_assert 16.0 "(and)" (Const (Sexpr (Bool true)));;
_assert 16.1 "(and e1)" (Var "e1");;
_assert 16.2 "(and e1 e2 e3 e4)"
  (If (Var "e1",
       If (Var "e2", If (Var "e3", Var "e4", Const (Sexpr (Bool false))),
	   Const (Sexpr (Bool false))),
       Const (Sexpr (Bool false))));;

(*Let* *)
_assert 17.0 "(let* () body)" (Applic (LambdaSimple ([], Var "body"), []));;
_assert 17.1 "(let* ((e1 v1)) body)" (Applic (LambdaSimple (["e1"], Var "body"), [Var "v1"]));;
_assert 17.2 "(let* ((e1 v1)(e2 v2)(e3 v3)) body)"
  (Applic (LambdaSimple (["e1"], Applic (LambdaSimple (["e2"], Applic (LambdaSimple (["e3"], Var "body"),
   [Var "v3"])), [Var "v2"])), [Var "v1"]));;


(*MIT define*)
(*
The body shouldn't be used in an applic expression.
_assert 18.0 "(define (var . arglst) . (body))" (Def (Var "var", LambdaOpt ([],"arglst", Applic (Var "body", []))));;*)

_assert 18.0 "(define (var . arglst) . (body))" (_tag_string "(define var (lambda arglst body))");;

(* equivalent to (define (var . arglst) body)  *)


(*Letrec*)
_assert 19.0 "(letrec ((f1 e1)(f2 e2)(f3 e3)) body)"
  (_tag_string
     "(let ((f1 'whatever)(f2 'whatever)(f3 'whatever))
(set! f1 e1) (set! f2 e2) (set! f3 e3)
(let () body))");;


    (*
This output is wrong as the 'body' of the letrec needs to be enclosed in a let expr according to the lectures
(Applic
 (LambdaSimple (["f1"; "f2"; "f3"],
   Seq
    [Set (Var "f1", Var "e1"); Set (Var "f2", Var "e2");
     Set (Var "f3", Var "e3"); Var "body"]),
 [Const (Sexpr (Symbol "whatever")); Const (Sexpr (Symbol "whatever"));
      Const (Sexpr (Symbol "whatever"))]));;*)


(*Quasiquote*)
_assert 20.0 "`,x" (_tag_string "x");;
_assertX 20.01 "`,@x";;
_assert 20.02 "`(a b)" (_tag_string "(cons 'a (cons 'b '()))");;
_assert 20.03 "`(,a b)" (_tag_string "(cons a (cons 'b '()))");;
_assert 20.04 "`(a ,b)" (_tag_string "(cons 'a (cons b '()))");;
_assert 20.05 "`(,@a b)" (_tag_string "(append a (cons 'b '()))");;
_assert 20.06 "`(a ,@b)" (_tag_string "(cons 'a (append b '()))");;
_assert 20.07 "`(,a ,@b)" (_tag_string "(cons a (append b '()))");;
_assert 20.08 "`(,@a ,@b)" (_tag_string "(append a (append b '()))");;
_assert 20.09 "`(,@a . ,b)" (_tag_string "(append a b)");;
_assert 20.10 "`(,a . ,@b)" (_tag_string "(cons a b)");;
_assert 20.11 "`(((,@a)))" (_tag_string "(cons (cons (append a '()) '()) '())");;
_assert 20.12 "`#(a ,b c ,d)" (_tag_string "(vector 'a b 'c d)");;
(*
_assert 20.15 "`" (_tag_string "");;
_assert 20.16 "`" (_tag_string "");;
  _assert 20.17 "`" (_tag_string "");;*)


(*Cond*)

(*
Before the fucking change that the rest of the ribs had to be enclosed in a lambda

_assert 21.0 "(cond (a => b)(c => d))"
  (_tag_string
     "(let ((value a)(f (lambda () b)))
        (if value
          ((f) value)
          (let ((value c)(f (lambda () d)))
            (if value
  ((f) value)))))");; *)

_assert 21.0 "(cond (e1 => f1) (e2 => f2))"
  (_tag_string
     "
(let
((value e1)
(f (lambda () f1))
(rest (lambda ()

(let ((value e2)(f (lambda () f2))) (if value ((f) value)))

)))
(if value ((f) value) (rest)))");;

(* Note: the separated line is the expansion of the second cond rib *)


_assert 21.1 "(cond (p1 e1 e2) (p2 e3 e4) (p3 e4 e5))"
  (_tag_string
     "(if p1
        (begin e1 e2)
        (if p2
          (begin e3 e4)
          (if p3
            (begin e4 e5))))");;

_assert 21.2 "(cond (p1 e1 e2) (p2 e3 e4) (else e5 e6) (BAD BAD BAD))"
  (_tag_string
     "(if p1
        (begin e1 e2)
        (if p2
          (begin e3 e4)
          (begin e5 e6)))");;

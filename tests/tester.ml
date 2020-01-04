#use "semantic-analyser.ml";;
open Tag_Parser;;
open Reader;;
open Semantics;;

(* general *)
    let fail_or_pass bool = 
    if bool then "passed" else "failed";;

    (* input: expr *)
        let printer output testnum expected_output =
            print_string (String.concat " " ["\n\n--PREV YEARS-- test num:"; 
            string_of_int testnum; 
            "\ntest output:";
            fail_or_pass (expected_output=output)]);;  

        let test_annotate_tail_calls_expr testnum input expected_output = 
            let output = annotate_tail_calls (annotate_lexical_addresses input) in
            printer output testnum expected_output;;

        let test_annotate_lexical_addresses_expr testnum input expected_output = 
            let output = (annotate_lexical_addresses input) in
            printer output testnum expected_output;;


    (* input: String *)
        let output_from_tag input = tag_parse_expression (read_sexpr input);;
        let ass3_runner input = run_semantics (output_from_tag input);;
        let test_ass3 input expected_output = 
        let output = (ass3_runner input) in
            print_string (String.concat " " ["\n\ntest input:"; 
            input; 
            "\ntest output:";
            fail_or_pass (expected_output=output)]);;

        let annotate_lexical_addresses_runner input = annotate_lexical_addresses (output_from_tag input);;
        let test_annotate_lexical_addresses_str input expected_output = 
        let output = (annotate_lexical_addresses_runner input) in
            print_string (String.concat " " ["\n\ntest input:"; 
            input; 
            "\ntest output:";
            fail_or_pass (expected_output=output)]);;   

        let annotate_tail_calls_runner input = annotate_tail_calls (annotate_lexical_addresses_runner input);;
        let test_annotate_tail_calls_str input expected_output = 
        let output = (annotate_tail_calls_runner input) in
            print_string (String.concat " " ["\n\ntest input:"; 
            input; 
            "\ntest output:";
            fail_or_pass (expected_output=output)]);;   

        let box_set_runner input = box_set (annotate_tail_calls_runner input);;
        let test_box_set input expected_output = 
        let output = (box_set_runner input) in
            print_string (String.concat " " ["\n\ntest input:"; 
            input; 
            "\ntest output:";
            fail_or_pass (expected_output=output)]);;   
(* end general *)

(* PREV YEARS *)
    test_annotate_lexical_addresses_expr 1
        (LambdaSimple ([], Const (Sexpr (Number (Int 1))))
        )
        (LambdaSimple' ([], Const' (Sexpr (Number (Int (1))))));;

    test_annotate_lexical_addresses_expr 2
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

    test_annotate_tail_calls_expr 3
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

    (* _assert 4
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
        (LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Or' ([Applic' (LambdaOpt' (["y"], "z", ApplicTP' (LambdaSimple' ([], ApplicTP' (LambdaSimple' ([], ApplicTP' (Var' (VarFree "+"), [BoxGet' (VarBound ("x", 2, 0));Var' (VarBound ("z", 1, 1))])), [])), [])), [BoxGet' (VarParam ("x", 0));Const' (Sexpr (Number (Int (1))))]);LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), Var' (VarFree "w")));ApplicTP' (Var' (VarFree "w"), [Var' (VarFree "w")])])])));; *)

    test_annotate_lexical_addresses_expr 5
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

    test_annotate_tail_calls_expr 6
        (LambdaOpt (["x"; "y"; "z"], "w",
        Seq
        [Var "z";
            Applic
            (LambdaSimple ([],
            Seq [Set (Var "w", Var "w"); Applic (Applic (Var "y", [Var "x"]), [])]),
            [])])
        )
        (LambdaOpt' (["x";"y";"z"], "w", Seq' ([Var' (VarParam ("z", 2));ApplicTP' (LambdaSimple' ([], Seq' ([Set' (Var' (VarBound ("w", 0, 3)), Var' (VarBound ("w", 0, 3)));ApplicTP' (Applic' (Var' (VarBound ("y", 0, 1)), [Var' (VarBound ("x", 0, 0))]), [])])), [])])));;

    test_annotate_lexical_addresses_expr 7
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

    (* _assert 8
        (LambdaSimple (["x"; "y"],
        Seq
        [Applic (Var "x", [Var "y"]);
            LambdaSimple ([],
            LambdaSimple ([],
            LambdaSimple ([],
            Set (Var "x",
                Applic (LambdaSimple (["z"], Set (Var "y", Var "x")), [Var "y"])))))])
        )
        (LambdaSimple' (["x";"y"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Set' (Var' (VarParam ("y", 1)), Box' (VarParam ("y", 1)));Seq' ([Applic' (BoxGet' (VarParam ("x", 0)), [BoxGet' (VarParam ("y", 1))]);LambdaSimple' ([], LambdaSimple' ([], LambdaSimple' ([], BoxSet' (VarBound ("x", 2, 0), Applic' (LambdaSimple' (["z"], BoxSet' (VarBound ("y", 3, 1), BoxGet' (VarBound ("x", 3, 0)))), [BoxGet' (VarBound ("y", 2, 1))])))))])])));; *)

    (* _assert 9
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
        (LambdaSimple' ([], Seq' ([Applic' (LambdaSimple' ([], Var' (VarFree "x")), []);Applic' (LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Seq' ([BoxSet' (VarParam ("x", 0), Const' (Sexpr (Number (Int (1)))));LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)))])])), [Const' (Sexpr (Number (Int (2))))]);ApplicTP' (LambdaOpt' ([], "x", Var' (VarParam ("x", 0))), [Const' (Sexpr (Number (Int (3))))])])));; *)

    (* _assert 10
        (LambdaSimple (["x"; "y"; "z"],
        Seq
        [LambdaSimple (["y"],
            Seq
            [Set (Var "x", Const (Sexpr (Number (Int 5))));
            Applic (Var "+", [Var "x"; Var "y"])]);
            Applic (Var "+", [Var "x"; Var "y"; Var "z"])])
        )
        (LambdaSimple' (["x";"y";"z"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Seq' ([LambdaSimple' (["y"], Seq' ([BoxSet' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Int (5)))));ApplicTP' (Var' (VarFree "+"), [BoxGet' (VarBound ("x", 0, 0));Var' (VarParam ("y", 0))])]));ApplicTP' (Var' (VarFree "+"), [BoxGet' (VarParam ("x", 0));Var' (VarParam ("y", 1));Var' (VarParam ("z", 2))])])])));; *)

    (* _assert 11
        (LambdaSimple (["x"], Set (Var "x", Applic (LambdaSimple ([], Var "x"), [])))
        )
        (LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));BoxSet' (VarParam ("x", 0), Applic' (LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0))), []))])));; *)

    (* _assert 12
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
        (Applic' (Var' (VarFree "y"), [LambdaSimple' (["y"], Seq' ([Set' (Var' (VarParam ("y", 0)), Box' (VarParam ("y", 0)));Seq' ([Set' (Var' (VarFree "a"), LambdaSimple' (["b"], ApplicTP' (Var' (VarFree "a"), [Var' (VarParam ("b", 0))])));Set' (Var' (VarFree "t"), LambdaSimple' (["x"], Seq' ([BoxSet' (VarBound ("y", 0, 0), LambdaSimple' (["j"], ApplicTP' (Var' (VarBound ("x", 0, 0)), [Var' (VarParam ("j", 0));Var' (VarBound ("x", 0, 0))])));Var' (VarFree "h")])));ApplicTP' (BoxGet' (VarParam ("y", 0)), [Var' (VarFree "a")])])]))]));; *)

    test_annotate_lexical_addresses_expr 13
        (LambdaSimple (["x"],
        Seq
        [LambdaSimple (["x"], Set (Var "x", Var "x"));
            LambdaSimple (["x"], Set (Var "x", Var "x"))])
        )
        (LambdaSimple' (["x"], Seq' ([LambdaSimple' (["x"], Set' (Var' (VarParam ("x", 0)), Var' (VarParam ("x", 0))));LambdaSimple' (["x"], Set' (Var' (VarParam ("x", 0)), Var' (VarParam ("x", 0))))])));;

    (* _assert 14
        (LambdaSimple (["x"; "y"],
        Seq
        [LambdaSimple ([], Set (Var "x", Var "y"));
            LambdaSimple ([], Set (Var "y", Var "x"))])
        )
        (LambdaSimple' (["x";"y"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Set' (Var' (VarParam ("y", 1)), Box' (VarParam ("y", 1)));Seq' ([LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), BoxGet' (VarBound ("y", 0, 1))));LambdaSimple' ([], BoxSet' (VarBound ("y", 0, 1), BoxGet' (VarBound ("x", 0, 0))))])])));; *)

    test_annotate_tail_calls_expr 15
        (LambdaOpt ([], "x",
        Seq
        [LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Int 1)))));
            Applic (Var "car", [Var "x"])])
        )
        (LambdaOpt' ([], "x", Seq' ([LambdaSimple' (["x"], Set' (Var' (VarParam ("x", 0)), Const' (Sexpr (Number (Int (1))))));ApplicTP' (Var' (VarFree "car"), [Var' (VarParam ("x", 0))])])));;

    test_annotate_lexical_addresses_expr 16
        (If (Var "x", Applic (Var "x", []), Var "x")
        )
        (If' (Var' (VarFree "x"), Applic' (Var' (VarFree "x"), []), Var' (VarFree "x")));;

    test_annotate_tail_calls_expr 17
        (LambdaSimple ([],
        If (Var "x", Applic (Var "x", []), Applic (Var "not", [Var "x"])))
        )
        (LambdaSimple' ([], If' (Var' (VarFree "x"), ApplicTP' (Var' (VarFree "x"), []), ApplicTP' (Var' (VarFree "not"), [Var' (VarFree "x")]))));;

    test_annotate_tail_calls_expr 18
        (LambdaSimple (["a"; "b"; "c"; "d"; "e"],
        Applic (Var "a",
        [Applic (Var "b", [Var "c"]); Applic (Var "c", [Var "b"; Var "d"]);
            Applic (Var "a",
            [Applic (Var "b", [Applic (Var "c", [Applic (Var "d", [Var "e"])])])])]))
        )
        (LambdaSimple' (["a";"b";"c";"d";"e"], ApplicTP' (Var' (VarParam ("a", 0)), [Applic' (Var' (VarParam ("b", 1)), [Var' (VarParam ("c", 2))]);Applic' (Var' (VarParam ("c", 2)), [Var' (VarParam ("b", 1));Var' (VarParam ("d", 3))]);Applic' (Var' (VarParam ("a", 0)), [Applic' (Var' (VarParam ("b", 1)), [Applic' (Var' (VarParam ("c", 2)), [Applic' (Var' (VarParam ("d", 3)), [Var' (VarParam ("e", 4))])])])])])));;

    test_annotate_lexical_addresses_expr 19
        (LambdaSimple (["x"],
        Seq [Applic (Var "x", []); Set (Var "x", Applic (Var "x", []))])
        )
        (LambdaSimple' (["x"], Seq' ([Applic' (Var' (VarParam ("x", 0)), []);Set' (Var' (VarParam ("x", 0)), Applic' (Var' (VarParam ("x", 0)), []))])));;

    test_annotate_tail_calls_expr 20
        (LambdaSimple (["x"],
        Applic
        (LambdaSimple (["y"],
            Seq [Set (Var "x", Applic (Var "y", [])); Const (Sexpr (Number (Int 2)))]),
        []))
        )
        (LambdaSimple' (["x"], ApplicTP' (LambdaSimple' (["y"], Seq' ([Set' (Var' (VarBound ("x", 0, 0)), Applic' (Var' (VarParam ("y", 0)), []));Const' (Sexpr (Number (Int (2))))])), [])));;

    test_annotate_lexical_addresses_expr 21
        (Const(Void)
        )
        (Const' (Void));;

    (* _assert 22
        (LambdaSimple (["x"],
        Seq
        [Var "x";
            LambdaSimple (["x"],
            Seq
            [Set (Var "x", Const (Sexpr (Number (Int 1))));
            LambdaSimple ([], Var "x")]);
            LambdaSimple ([], Set (Var "x", Var "x"))])
        )
        (LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Seq' ([BoxGet' (VarParam ("x", 0));LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Seq' ([BoxSet' (VarParam ("x", 0), Const' (Sexpr (Number (Int (1)))));LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)))])]));LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), BoxGet' (VarBound ("x", 0, 0))))])])));; *)

    (* _assert 23
        (LambdaSimple (["x"],
        Seq
        [Var "x";
            LambdaSimple (["x"],
            Seq
            [Set (Var "y", Var "x");
            LambdaSimple ([], Var "x")]);
            LambdaSimple ([], Set (Var "x", Var "x"))])
        )
        (LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Seq' ([BoxGet' (VarParam ("x", 0));LambdaSimple' (["x"], Seq' ([Set' (Var' (VarFree "y"), Var' (VarParam ("x", 0)));LambdaSimple' ([], Var' (VarBound ("x", 0, 0)))]));LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), BoxGet' (VarBound ("x", 0, 0))))])])));; *)

(* END PREV YEARS *)

(* annotate_lexical_addresses tests *)
    test_annotate_lexical_addresses_str 
        "(lambda (x y) 
            (x (lambda (t)
                    (t (x y))))
            v)"
            (LambdaSimple' (["x"; "y"],
                Seq'
                [Applic' (Var' (VarParam ("x", 0)),
                    [LambdaSimple' (["t"],
                    Applic' (Var' (VarParam ("t", 0)),
                    [Applic' (Var' (VarBound ("x", 0, 0)),
                        [Var' (VarBound ("y", 0, 1))])]))]);
                Var' (VarFree "v")]));;

    test_annotate_lexical_addresses_str 
        "(lambda (x)
                    (lambda (y z)
                    (lambda (t)
                    x)))"
            (LambdaSimple' (["x"],
                LambdaSimple' (["y"; "z"],
                LambdaSimple' (["t"], Var' (VarBound ("x", 1, 0))))));;


    test_annotate_lexical_addresses_str 
        "z" (Var' (VarFree "z"));;

    test_annotate_lexical_addresses_str 
        "(lambda (a) (a (a (lambda (b) (b (b (a c)))))))"
        (LambdaSimple' (["a"],
        Applic' (Var' (VarParam ("a", 0)),
        [Applic' (Var' (VarParam ("a", 0)),
        [LambdaSimple' (["b"],
        Applic' (Var' (VarParam ("b", 0)),
        [Applic' (Var' (VarParam ("b", 0)),
        [Applic' (Var' (VarBound ("a", 0, 0)),
        [Var' (VarFree "c")])])]))])])));;

    test_annotate_lexical_addresses_str 
            "(lambda (x)
            (f (lambda (y)
            (g x y))))"
            (LambdaSimple' (["x"],
                Applic' (Var' (VarFree "f"),
                [LambdaSimple' (["y"],
                    Applic' (Var' (VarFree "g"),
                    [Var' (VarBound ("x", 0, 0)); Var' (VarParam ("y", 0))]))])));;

    test_annotate_lexical_addresses_str "
        (lambda (x y)
            (x (lambda (x y)
                (g (lambda (x y)
                    (x y))))))
            "
            (LambdaSimple' (["x"; "y"],
                Applic' (Var' (VarParam ("x", 0)),
                [LambdaSimple' (["x"; "y"],
                    Applic' (Var' (VarFree "g"),
                    [LambdaSimple' (["x"; "y"],
                    Applic' (Var' (VarParam ("x", 0)),
                        [Var' (VarParam ("y", 1))]))]))])));;
(* end annotate_lexical_addresses tests *)

(* annotate_tail_calls *)
    test_annotate_tail_calls_str
        "(lambda (x y) (x))"
        (LambdaSimple' (["x"; "y"],
        ApplicTP' (Var' (VarParam ("x", 0)), [])));;

    test_annotate_tail_calls_str
        "(lambda (x)
        (f (g (g x))))"
        (LambdaSimple' (["x"],
            ApplicTP' (Var' (VarFree "f"),
            [Applic' (Var' (VarFree "g"),
                [Applic' (Var' (VarFree "g"), [Var' (VarParam ("x", 0))])])])))
        ;;

    test_annotate_tail_calls_str
        "(lambda (x)
        (f (lambda (y)
        (g x y))))"
        (LambdaSimple' (["x"],
            ApplicTP' (Var' (VarFree "f"),
            [LambdaSimple' (["y"],
                ApplicTP' (Var' (VarFree "g"),
                [Var' (VarBound ("x", 0, 0)); Var' (VarParam ("y", 0))]))])))
        ;;

    test_annotate_tail_calls_str
        "(lambda (x y z w)
        (if (foo? x)
        (goo y)
        (boo (doo z))))"
        (LambdaSimple' (["x"; "y"; "z"; "w"],
            If'
            (Applic' (Var' (VarFree "foo?"),
                [Var' (VarParam ("x", 0))]),
            ApplicTP' (Var' (VarFree "goo"),
            [Var' (VarParam ("y", 1))]),
            ApplicTP' (Var' (VarFree "boo"),
            [Applic' (Var' (VarFree "doo"),
                [Var' (VarParam ("z", 2))])]))));;

    test_annotate_tail_calls_str
        "(lambda (x y z)
        (f (if (g? x)
        (h y)
        (w z))))"
        (LambdaSimple' (["x"; "y"; "z"],
            ApplicTP' (Var' (VarFree "f"),
            [If'
                (Applic' (Var' (VarFree "g?"),
                [Var' (VarParam ("x", 0))]),
                Applic' (Var' (VarFree "h"), [Var' (VarParam ("y", 1))]),
                Applic' (Var' (VarFree "w"), [Var' (VarParam ("z", 2))]))])))
                    ;;

    test_annotate_tail_calls_str
        "(lambda (a b)
        (f a)
        (g a b)
        (display \"done!\\n\"))"
        (LambdaSimple' (["a"; "b"],
            Seq'
            [Applic' (Var' (VarFree "f"), [Var' (VarParam ("a", 0))]);
            Applic' (Var' (VarFree "g"),
                [Var' (VarParam ("a", 0)); Var' (VarParam ("b", 1))]);
            ApplicTP' (Var' (VarFree "display"),
                [Const' (Sexpr (String "done!\n"))])]));;

    test_annotate_tail_calls_str
        "(lambda ()
        (and (f x) (g y) (h z)))"
        (LambdaSimple' ([],
        If' (Applic' (Var' (VarFree "f"), [Var' (VarFree "x")]),
        If' (Applic' (Var' (VarFree "g"), [Var' (VarFree "y")]),
        ApplicTP' (Var' (VarFree "h"), [Var' (VarFree "z")]),
        Const' (Sexpr (Bool false))),
        Const' (Sexpr (Bool false)))))
        ;;

    test_annotate_tail_calls_str
        "(lambda ()
            (or (f (g x)) y))"
            (LambdaSimple' ([],
            Or'
            [Applic' (Var' (VarFree "f"),
                [Applic' (Var' (VarFree "g"), [Var' (VarFree "x")])]);
            Var' (VarFree "y")]));;

    test_annotate_tail_calls_str
        "(lambda ()
        (set! x (f y)))"
        (LambdaSimple' ([],
        Set' (Var' (VarFree "x"),
        Applic' (Var' (VarFree "f"), [Var' (VarFree "y")]))));;

    test_annotate_tail_calls_str
        "(lambda ()
        (set! x (f (lambda (y)
        (g x y)))))"
        (LambdaSimple' ([],
        Set' (Var' (VarFree "x"),
        Applic' (Var' (VarFree "f"),
        [LambdaSimple' (["y"],
            ApplicTP' (Var' (VarFree "g"),
            [Var' (VarFree "x"); Var' (VarParam ("y", 0))]))]))));;
    
    test_annotate_tail_calls_str
        "(lambda (x y z)
        (cond ((f? x) (g y))
        ((g? x) (f x) (f y))
        (else (h x) (f y) (g (f x)))))"
        (LambdaSimple' (["x"; "y"; "z"],
        If'
        (Applic' (Var' (VarFree "f?"), [Var' (VarParam ("x", 0))]),
        ApplicTP' (Var' (VarFree "g"), [Var' (VarParam ("y", 1))]),
        If'
        (Applic' (Var' (VarFree "g?"), [Var' (VarParam ("x", 0))]),
        Seq'
            [Applic' (Var' (VarFree "f"), [Var' (VarParam ("x", 0))]);
            ApplicTP' (Var' (VarFree "f"),
            [Var' (VarParam ("y", 1))])],
        Seq'
            [Applic' (Var' (VarFree "h"), [Var' (VarParam ("x", 0))]);
            Applic' (Var' (VarFree "f"), [Var' (VarParam ("y", 1))]);
            ApplicTP' (Var' (VarFree "g"),
            [Applic' (Var' (VarFree "f"),
                [Var' (VarParam ("x", 0))])])]))));;

    test_annotate_tail_calls_str
        "(let ((x (f y))
        (y (g x)))
        (goo (boo x) y))"
        (Applic'
            (LambdaSimple' (["x"; "y"],
            ApplicTP' (Var' (VarFree "goo"),
                [Applic' (Var' (VarFree "boo"),
                [Var' (VarParam ("x", 0))]);
                Var' (VarParam ("y", 1))])),
            [Applic' (Var' (VarFree "f"), [Var' (VarFree "y")]);
            Applic' (Var' (VarFree "g"), [Var' (VarFree "x")])]));;

    test_annotate_tail_calls_str
        "(lambda (s)
        (apply f s))"
        (LambdaSimple' (["s"],
        ApplicTP' (Var' (VarFree "apply"),
        [Var' (VarFree "f"); Var' (VarParam ("s", 0))])));
(* end annotate_tail_calls *)

(* reader.ml
 * A compiler from Scheme to x86/64
 *
 * Programmer: Yehonatan Peleg And Meytal Rose, 2018
 *)

#use "reader.ml";;

let  red =    "\027[38;5;196m"
let  grn =  "\027[38;5;082m"
let yel  = "\027[38;5;226m"
let mag  = "\027[38;5;201m"
let reset =  "\027[0m"

exception Fatal_error of string;;
exception TestException of string;;

let green_tests = ref 0
let red_tests = ref 0
let current_test = ref "No Current Test"
let failure_info = ref "as not as expected"
let got = ref "Not A Real Got"
let expected = ref "Not A Real Expected"

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(n1), Number(n2) -> n1 = n2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | _ -> false;;

let rec float_sexpr_eq s1 s2 =
  match s1, s2 with
  | Number(Float(e1)) , Number(Float(e2)) -> (Printf.sprintf "%f" e1) = (Printf.sprintf "%f" e2)
  | _ -> false

let rec sexpr_eq_as_list sexprList1 sexprList2 = 
   match sexprList1, sexprList2 with
   | [] , [] -> true
   | [] , head2 :: tail2 -> false
   | head1 :: tail1 , [] -> false
   | head1 :: tail1 , head2 :: tail2 -> (sexpr_eq head1 head2) && (sexpr_eq_as_list tail1 tail2)

let prompt = fun title -> Printf.printf "%s*******************************************\n                     %s                  \n*******************************************\n%s" mag title mag;;

let start_prompt = fun () -> prompt "Start"; Printf.printf "\n";;

let end_prompt = fun () -> Printf.printf "\n"; prompt "End"; Printf.printf "%sGreen: %d%s%s Red: %d%s\n" grn !green_tests reset red !red_tests reset;;

let test = fun test_number equal_test -> 
    if equal_test 
        then 
            green_tests := !green_tests + 1 
        else
            (red_tests := !red_tests + 1;
            (if !failure_info = "as not as expected" then failure_info := Printf.sprintf "with got %s while expected %s" !got !expected);
            Printf.printf "%s%s number %d Failed %s !!!%s\n" red !current_test test_number !failure_info red);
            failure_info := "as not as expected";;

let rec print_sexpr = fun sexprObj ->
  match sexprObj  with
    | Bool(true) -> "Bool(true)"
    | Bool(false) -> "Bool(false)"
    | Nil -> "Nill"
    | Number(Int(e)) -> Printf.sprintf "Number(Int(%d))" e
    | Number(Float(e)) -> Printf.sprintf "Number(Float(%f))" e
    | Char(e) -> Printf.sprintf "Char(%c)" e
    | String(e) -> Printf.sprintf "String(%s)" e
    | Symbol(e) -> Printf.sprintf "Symbol(%s)" e
    | Pair(e,s) -> Printf.sprintf "Pair(%s,%s)" (print_sexpr e) (print_sexpr s) 

and 

print_sexprs = fun sexprList -> 
  match sexprList with
    | [] -> ""
    | head :: tail -> (print_sexpr head) ^ "," ^ (print_sexprs tail)

let print_sexprs_as_list = fun sexprList ->
  let sexprsString = print_sexprs sexprList in
    "[ " ^ sexprsString ^ " ]";;

let execute_read_sexpr = fun string ->
  let return = try (Reader.read_sexpr string)
               with PC.X_no_match -> failure_info := "with " ^ string ^ " as X_no_match"; String "test failed" in
  (got := print_sexpr return;
  return);;

let execute_expected = fun sexprObj -> 
  expected := print_sexpr sexprObj;
  sexprObj;;

let execute_read_sexprs_as_list = fun string ->
  let return = try (Reader.read_sexprs string)
               with PC.X_no_match -> failure_info := "with " ^ string ^ " as X_no_match"; [String("test failed")] in
  (got := print_sexprs_as_list return;
  return);;

let execute_expected_as_list = fun sexprObj -> 
  expected := print_sexprs_as_list sexprObj;
  sexprObj;;

let test_Boolean_ = fun () ->
    current_test := "test_Boolean_";
    test 1 (sexpr_eq (execute_read_sexpr "#t") (execute_expected(Bool true)));
    test 2 (sexpr_eq (execute_read_sexpr "#f") (execute_expected(Bool false)));
    test 3 (sexpr_eq (execute_read_sexpr " #F") (execute_expected(Bool false)));
    test 4 (sexpr_eq (execute_read_sexpr " #t") (execute_expected(Bool true)));
    test 5 (sexpr_eq (execute_read_sexpr " #T ") (execute_expected(Bool true)));
    test 6 (sexpr_eq (execute_read_sexpr " #f ") (execute_expected(Bool false)));
    ;;

let test_Char_ = fun () ->
    current_test := "test_Char_";
    test 1 (sexpr_eq (execute_read_sexpr "#\\a") (execute_expected(Char 'a')));
    test 2 (sexpr_eq (execute_read_sexpr "#\\g") (execute_expected(Char 'g')));
    test 3 (sexpr_eq (execute_read_sexpr "#\\4") (execute_expected(Char '4')));
    test 4 (sexpr_eq (execute_read_sexpr "#\\#") (execute_expected(Char '#')));
    test 5 (sexpr_eq (execute_read_sexpr "#\\newline") (execute_expected(Char '\n')));
    test 6 (sexpr_eq (execute_read_sexpr "#\\nul") (execute_expected(Char (Char.chr 0))));
    test 7 (sexpr_eq (execute_read_sexpr "#\\return") (execute_expected(Char '\r')));
    test 8 (sexpr_eq (execute_read_sexpr "#\\space") (execute_expected(Char ' ')));
    test 9 (sexpr_eq (execute_read_sexpr "#\\tab") (execute_expected(Char '\t')));
    test 13 (sexpr_eq (execute_read_sexpr "#\\A") (execute_expected(Char 'A')));
    test 14 (sexpr_eq (execute_read_sexpr "#\\Z") (execute_expected(Char 'Z')));
    test 15 (sexpr_eq (execute_read_sexpr "#\\z") (execute_expected(Char 'z')));
    test 16 (sexpr_eq (execute_read_sexpr "#\\O") (execute_expected(Char 'O')));
  ;;

let test_Number_ = fun () ->
    current_test := "test_Number_";
    test 1 (sexpr_eq (execute_read_sexpr "4") (execute_expected(Number (Int 4))));
    test 2 (sexpr_eq (execute_read_sexpr "10") (execute_expected(Number (Int 10))));
    test 3 (sexpr_eq (execute_read_sexpr "+10") (execute_expected(Number (Int 10))));
    test 4 (sexpr_eq (execute_read_sexpr "-10") (execute_expected(Number (Int (-10)))));
    test 5 (sexpr_eq (execute_read_sexpr "-34324324324324") (execute_expected(Number (Int (-34324324324324)))));
    test 6 (sexpr_eq (execute_read_sexpr "-10.99") (execute_expected(Number (Float (-10.99)))));
    test 9 (sexpr_eq (execute_read_sexpr "10.99") (execute_expected(Number (Float (10.99)))));
    test 13 (sexpr_eq (execute_read_sexpr "-0.4321") (execute_expected(Number (Float (-0.4321)))));
    test 15 (sexpr_eq (execute_read_sexpr "-0.32123") (execute_expected(Number (Float (-0.32123)))));
    test 16 (sexpr_eq (execute_read_sexpr "-40.32123") (execute_expected(Number (Float (-40.32123)))));
    test 17 (sexpr_eq (execute_read_sexpr "40.32123") (execute_expected(Number (Float (40.32123)))));
    ;;

let test_String_ = fun () ->
    current_test := "test_String_";
    test 1 (sexpr_eq (execute_read_sexpr "\"This is a string\"") (execute_expected(String "This is a string")));
    test 2 (sexpr_eq (execute_read_sexpr "\"This is a string with \\\\ \"") (execute_expected(String "This is a string with \\ ")));
    test 3 (sexpr_eq (execute_read_sexpr "\"This is a string with \\\" \"") ((execute_expected(String "This is a string with \" "))));
    test 4 (sexpr_eq (execute_read_sexpr "\"This is a string with \\t \"") ((execute_expected(String "This is a string with \t "))));
    test 5 (sexpr_eq (execute_read_sexpr "\"This is a string with \\f \"") ((execute_expected(String (Printf.sprintf "This is a string with %c " (Char.chr 12))))));
    test 6 (sexpr_eq (execute_read_sexpr "\"This is a string with \\r \"") ((execute_expected(String "This is a string with \r "))));
    test 7 (sexpr_eq (execute_read_sexpr "\"This is a string with \\n \"") ((execute_expected(String "This is a string with \n "))));
    ;;

(* (0 | · · · | 9) | (a | · · · | z) | (A | · · · | Z) | ! | $
| ^ | * | - | _ | = | + | < | > | ? | / | : *)
let test_Symbol_ = fun () ->
    current_test := "test_Symbol_";
    test 1 (sexpr_eq (execute_read_sexpr "a") (execute_expected(Symbol "a")));
    test 2 (sexpr_eq (execute_read_sexpr "aaaa") (execute_expected(Symbol "aaaa")));
    test 3 (sexpr_eq (execute_read_sexpr "bbbb") (execute_expected(Symbol "bbbb")));
    test 4 (sexpr_eq (execute_read_sexpr "abcdef") (execute_expected(Symbol "abcdef")));
    test 5 (sexpr_eq (execute_read_sexpr "abcdefghijklmnop") (execute_expected(Symbol "abcdefghijklmnop")));
    test 6 (sexpr_eq (execute_read_sexpr "abcdefghijklmnopqrstuvwxyz") (execute_expected(Symbol "abcdefghijklmnopqrstuvwxyz")));
    test 7 (sexpr_eq (execute_read_sexpr "abcdefghijklmnopqrstuvwxyz0123456789") (execute_expected(Symbol "abcdefghijklmnopqrstuvwxyz0123456789")));
    test 8 (sexpr_eq (execute_read_sexpr "!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456789") (execute_expected(Symbol "!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456789")));
    test 9 (sexpr_eq (execute_read_sexpr "!$^*-_=+<>?/:ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") (execute_expected(Symbol "!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456789")));
    test 10 (sexpr_eq (execute_read_sexpr "ZzZzZzZz0123456789") (execute_expected(Symbol "zzzzzzzz0123456789")));
    test 11 (sexpr_eq (execute_read_sexpr "aBhThKtUlKyGtGBVFN") (execute_expected(Symbol "abhthktulkygtgbvfn")));
    test 12 (sexpr_eq (execute_read_sexpr "!$^*-_=+<>?/:0123456789aBcDeFgHiJK") (execute_expected(Symbol "!$^*-_=+<>?/:0123456789abcdefghijk")));
    ;;

let test_List_ = fun () ->
    current_test := "test_List_";
    test 1 (sexpr_eq (execute_read_sexpr "()") (execute_expected(Nil)));
    test 2 (sexpr_eq (execute_read_sexpr "( #t )") (execute_expected(Pair(Bool(true), Nil))));
    test 3 (sexpr_eq (execute_read_sexpr "( \"this\" )") (execute_expected(Pair(String("this"), Nil))));
    test 4 (sexpr_eq (execute_read_sexpr "( 37392 )") (execute_expected(Pair(Number(Int(37392)), Nil))));
    test 5 (sexpr_eq (execute_read_sexpr "( 37392.39382 )") (execute_expected(Pair(Number(Float(37392.39382)), Nil))));
    test 6 (sexpr_eq (execute_read_sexpr "( #\\c )") (execute_expected(Pair(Char('c'), Nil))));
    test 7 (sexpr_eq (execute_read_sexpr "( #\\c 37392.39382 )") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Nil)))));
    test 8 (sexpr_eq (execute_read_sexpr "( #\\c 37392.39382 37392 )") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Nil))))));
    test 9 (sexpr_eq (execute_read_sexpr "( #\\c 37392.39382 37392 \"this\" )") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Nil)))))));
    test 10 (sexpr_eq (execute_read_sexpr "(#\\c 37392.39382 37392 \"this\" #t)") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Pair(Bool(true), Nil))))))));
    test 13 (sexpr_eq (execute_read_sexpr "(    )") (execute_expected(Nil)));
    ;;

let test_DottedList_ = fun () ->
    current_test := "test_DottedList_";
    test 1 (sexpr_eq (execute_read_sexpr "(#t . #f)") (execute_expected(Pair(Bool(true),Bool(false)))));
    test 2 (sexpr_eq (execute_read_sexpr "(#t . asfsfdsfa)") (execute_expected(Pair(Bool(true),Symbol("asfsfdsfa")))));
    test 5 (sexpr_eq (execute_read_sexpr "( #\\c . 37392.39382 )") (execute_expected(Pair(Char('c'), Number(Float(37392.39382))))));
    test 6 (sexpr_eq (execute_read_sexpr "( #\\c 37392.39382 . 37392 )") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Number(Int(37392)))))));
    test 7 (sexpr_eq (execute_read_sexpr "( #\\c 37392.39382 37392 . \"this\" )") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), String("this")))))));
    ;;


let test_Quoted_ = fun () ->
    current_test := "test_Quoted_";
    test 1 (sexpr_eq (execute_read_sexpr "'#t") (execute_expected(Pair(Symbol("quote"),Pair(Bool(true),Nil)))));
    test 2 (sexpr_eq (execute_read_sexpr "'#f") (execute_expected(Pair(Symbol("quote"),Pair(Bool(false),Nil)))));
    test 3 (sexpr_eq (execute_read_sexpr "'#\\a") (execute_expected(Pair(Symbol("quote"),Pair(Char('a'),Nil)))));
    test 4 (sexpr_eq (execute_read_sexpr "'#\\g") (execute_expected(Pair(Symbol("quote"),Pair(Char('g'),Nil)))));
    test 5 (sexpr_eq (execute_read_sexpr "'#\\4") (execute_expected(Pair(Symbol("quote"),Pair(Char('4'),Nil)))));
    test 6 (sexpr_eq (execute_read_sexpr "'#\\#") (execute_expected(Pair(Symbol("quote"),Pair(Char('#'),Nil)))));
    test 7 (sexpr_eq (execute_read_sexpr "'#\\newline") (execute_expected(Pair(Symbol("quote"),Pair(Char('\n'),Nil)))));
    test 8 (sexpr_eq (execute_read_sexpr "'#\\nul") (execute_expected(Pair(Symbol("quote"),Pair(Char(Char.chr 0),Nil)))));
    test 9 (sexpr_eq (execute_read_sexpr "'#\\return") (execute_expected(Pair(Symbol("quote"),Pair(Char('\r'),Nil)))));
    test 10 (sexpr_eq (execute_read_sexpr "'#\\space") (execute_expected(Pair(Symbol("quote"),Pair(Char(' '),Nil)))));
    test 11 (sexpr_eq (execute_read_sexpr "'#\\tab") (execute_expected(Pair(Symbol("quote"),Pair(Char('\t'),Nil)))));
    test 15 (sexpr_eq (execute_read_sexpr "'4") (execute_expected(Pair(Symbol("quote"),Pair(Number(Int 4),Nil)))));
    test 16 (sexpr_eq (execute_read_sexpr "'10") (execute_expected(Pair(Symbol("quote"),Pair(Number (Int 10),Nil)))));
    test 17 (sexpr_eq (execute_read_sexpr "'+10") (execute_expected(Pair(Symbol("quote"),Pair(Number (Int 10),Nil)))));
    test 18 (sexpr_eq (execute_read_sexpr "'-10") (execute_expected(Pair(Symbol("quote"),Pair(Number(Int(-10)),Nil)))));
    test 19 (sexpr_eq (execute_read_sexpr "'-34324324324324") (execute_expected(Pair(Symbol("quote"),Pair(Number(Int(-34324324324324)),Nil)))));
    test 20 (sexpr_eq (execute_read_sexpr "'-10.99") (execute_expected(Pair(Symbol("quote"),Pair(Number(Float(-10.99)),Nil)))));
    test 23 (sexpr_eq (execute_read_sexpr "'10.99") (execute_expected(Pair(Symbol("quote"),Pair(Number(Float(10.99)),Nil)))));
    test 25 (sexpr_eq (execute_read_sexpr "'\"This is a string\"") (execute_expected(Pair(Symbol("quote"),Pair(String("This is a string"),Nil)))));
    test 26 (sexpr_eq (execute_read_sexpr "'\"This is a string with \\\\ \"") (execute_expected(Pair(Symbol("quote"),Pair(String("This is a string with \\ "),Nil)))));
    test 27 (sexpr_eq (execute_read_sexpr "'\"This is a string with \\\" \"") (execute_expected(Pair(Symbol("quote"),Pair(String("This is a string with \" "),Nil)))));
    test 28 (sexpr_eq (execute_read_sexpr "'\"This is a string with \\t \"") (execute_expected(Pair(Symbol("quote"),Pair(String("This is a string with \t "),Nil)))));
    test 29 (sexpr_eq (execute_read_sexpr "'\"This is a string with \\f \"") (execute_expected(Pair(Symbol("quote"),Pair(String(Printf.sprintf "This is a string with %c " (Char.chr 12)),Nil)))));
    test 30 (sexpr_eq (execute_read_sexpr "'\"This is a string with \\r \"") (execute_expected(Pair(Symbol("quote"),Pair(String("This is a string with \r "),Nil)))));
    test 31 (sexpr_eq (execute_read_sexpr "'\"This is a string with \\n \"") (execute_expected(Pair(Symbol("quote"),Pair(String("This is a string with \n "),Nil)))));
    test 36 (sexpr_eq (execute_read_sexpr "'a") (execute_expected(Pair(Symbol("quote"),Pair(Symbol("a"),Nil)))));
    test 37 (sexpr_eq (execute_read_sexpr "'aaaa") (execute_expected(Pair(Symbol("quote"),Pair(Symbol("aaaa"),Nil)))));
    test 38 (sexpr_eq (execute_read_sexpr "'bbbb") (execute_expected(Pair(Symbol("quote"),Pair(Symbol("bbbb"),Nil)))));
    test 39 (sexpr_eq (execute_read_sexpr "'abcdef") (execute_expected(Pair(Symbol("quote"),Pair(Symbol("abcdef"),Nil)))));
    test 40 (sexpr_eq (execute_read_sexpr "'abcdefghijklmnop") (execute_expected(Pair(Symbol("quote"),Pair(Symbol("abcdefghijklmnop"),Nil)))));
    test 41 (sexpr_eq (execute_read_sexpr "'abcdefghijklmnopqrstuvwxyz") (execute_expected(Pair(Symbol("quote"),Pair((Symbol("abcdefghijklmnopqrstuvwxyz"),Nil))))));
    test 42 (sexpr_eq (execute_read_sexpr "'abcdefghijklmnopqrstuvwxyz0123456789") (execute_expected(Pair(Symbol("quote"),Pair(Symbol("abcdefghijklmnopqrstuvwxyz0123456789"),Nil)))));
    test 43 (sexpr_eq (execute_read_sexpr "'!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456789") (execute_expected(Pair(Symbol("quote"),Pair(Symbol("!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456789"),Nil)))));
    test 44 (sexpr_eq (execute_read_sexpr "'()") (execute_expected(Pair(Symbol("quote"),Pair(Nil,Nil)))));
    test 45 (sexpr_eq (execute_read_sexpr "'( #t )") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Bool(true), Nil),Nil)))));
    test 46 (sexpr_eq (execute_read_sexpr "'( \"this\" )") (execute_expected(Pair(Symbol("quote"),Pair(Pair(String("this"),Nil),Nil)))));
    test 47 (sexpr_eq (execute_read_sexpr "'( 37392 )") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Number(Int(37392)),Nil),Nil)))));
    test 48 (sexpr_eq (execute_read_sexpr "'( 37392.39382 )") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Number(Float(37392.39382)),Nil),Nil)))));
    test 49 (sexpr_eq (execute_read_sexpr "'( #\\c )") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Char('c'), Nil),Nil)))));
    test 50 (sexpr_eq (execute_read_sexpr "'( #\\c 37392.39382 )") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Nil)),Nil)))));
    test 51 (sexpr_eq (execute_read_sexpr "'( #\\c 37392.39382 37392 )") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Nil))),Nil)))));
    test 52 (sexpr_eq (execute_read_sexpr "'( #\\c 37392.39382 37392 \"this\" )") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Nil)))),Nil)))));
    test 53 (sexpr_eq (execute_read_sexpr "'(#\\c 37392.39382 37392 \"this\" #t)") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Pair(Bool(true), Nil))))),Nil)))));
    test 54 (sexpr_eq (execute_read_sexpr "'(#t . #f)") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Bool(true),Bool(false)),Nil)))));
    test 55 (sexpr_eq (execute_read_sexpr "'(#t . asfsfdsfa)") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Bool(true),Symbol("asfsfdsfa")),Nil)))));
    test 58 (sexpr_eq (execute_read_sexpr "'( #\\c . 37392.39382 )") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Char('c'), Number(Float(37392.39382))),Nil)))));
    test 59 (sexpr_eq (execute_read_sexpr "'( #\\c 37392.39382 . 37392 )") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Number(Int(37392)))),Nil)))));
    test 60 (sexpr_eq (execute_read_sexpr "'( #\\c 37392.39382 37392 . \"this\" )") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), String("this")))),Nil)))));
    test 61 (sexpr_eq (execute_read_sexpr "'(#\\c 37392.39382 37392 \"this\" . #t)") (execute_expected(Pair(Symbol("quote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Bool(true))))),Nil)))));
    ;;

let test_QuasiQuoted_ = fun () ->
    current_test := "test_QuasiQuoted_";
    test 1 (sexpr_eq (execute_read_sexpr "`#t") (execute_expected(Pair(Symbol("quasiquote"),Pair(Bool(true),Nil)))));
    test 2 (sexpr_eq (execute_read_sexpr "`#f") (execute_expected(Pair(Symbol("quasiquote"),Pair(Bool(false),Nil)))));
    test 3 (sexpr_eq (execute_read_sexpr "`#\\a") (execute_expected(Pair(Symbol("quasiquote"),Pair(Char('a'),Nil)))));
    test 4 (sexpr_eq (execute_read_sexpr "`#\\g") (execute_expected(Pair(Symbol("quasiquote"),Pair(Char('g'),Nil)))));
    test 5 (sexpr_eq (execute_read_sexpr "`#\\4") (execute_expected(Pair(Symbol("quasiquote"),Pair(Char('4'),Nil)))));
    test 6 (sexpr_eq (execute_read_sexpr "`#\\#") (execute_expected(Pair(Symbol("quasiquote"),Pair(Char('#'),Nil)))));
    test 7 (sexpr_eq (execute_read_sexpr "`#\\newline") (execute_expected(Pair(Symbol("quasiquote"),Pair(Char('\n'),Nil)))));
    test 8 (sexpr_eq (execute_read_sexpr "`#\\nul") (execute_expected(Pair(Symbol("quasiquote"),Pair(Char(Char.chr 0),Nil)))));
    test 9 (sexpr_eq (execute_read_sexpr "`#\\return") (execute_expected(Pair(Symbol("quasiquote"),Pair(Char('\r'),Nil)))));
    test 10 (sexpr_eq (execute_read_sexpr "`#\\space") (execute_expected(Pair(Symbol("quasiquote"),Pair(Char(' '),Nil)))));
    test 11 (sexpr_eq (execute_read_sexpr "`#\\tab") (execute_expected(Pair(Symbol("quasiquote"),Pair(Char('\t'),Nil)))));
    test 15 (sexpr_eq (execute_read_sexpr "`4") (execute_expected(Pair(Symbol("quasiquote"),Pair(Number(Int 4),Nil)))));
    test 16 (sexpr_eq (execute_read_sexpr "`10") (execute_expected(Pair(Symbol("quasiquote"),Pair(Number (Int 10),Nil)))));
    test 17 (sexpr_eq (execute_read_sexpr "`+10") (execute_expected(Pair(Symbol("quasiquote"),Pair(Number (Int 10),Nil)))));
    test 18 (sexpr_eq (execute_read_sexpr "`-10") (execute_expected(Pair(Symbol("quasiquote"),Pair(Number(Int(-10)),Nil)))));
    test 19 (sexpr_eq (execute_read_sexpr "`-34324324324324") (execute_expected(Pair(Symbol("quasiquote"),Pair(Number(Int(-34324324324324)),Nil)))));
    test 20 (sexpr_eq (execute_read_sexpr "`-10.99") (execute_expected(Pair(Symbol("quasiquote"),Pair(Number(Float(-10.99)),Nil)))));
    test 23 (sexpr_eq (execute_read_sexpr "`10.99") (execute_expected(Pair(Symbol("quasiquote"),Pair(Number(Float(10.99)),Nil)))));
    test 25 (sexpr_eq (execute_read_sexpr "`\"This is a string\"") (execute_expected(Pair(Symbol("quasiquote"),Pair(String("This is a string"),Nil)))));
    test 26 (sexpr_eq (execute_read_sexpr "`\"This is a string with \\\\ \"") (execute_expected(Pair(Symbol("quasiquote"),Pair(String("This is a string with \\ "),Nil)))));
    test 27 (sexpr_eq (execute_read_sexpr "`\"This is a string with \\\" \"") (execute_expected(Pair(Symbol("quasiquote"),Pair(String("This is a string with \" "),Nil)))));
    test 28 (sexpr_eq (execute_read_sexpr "`\"This is a string with \\t \"") (execute_expected(Pair(Symbol("quasiquote"),Pair(String("This is a string with \t "),Nil)))));
    test 29 (sexpr_eq (execute_read_sexpr "`\"This is a string with \\f \"") (execute_expected(Pair(Symbol("quasiquote"),Pair(String(Printf.sprintf "This is a string with %c " (Char.chr 12)),Nil)))));
    test 30 (sexpr_eq (execute_read_sexpr "`\"This is a string with \\r \"") (execute_expected(Pair(Symbol("quasiquote"),Pair(String("This is a string with \r "),Nil)))));
    test 31 (sexpr_eq (execute_read_sexpr "`\"This is a string with \\n \"") (execute_expected(Pair(Symbol("quasiquote"),Pair(String("This is a string with \n "),Nil)))));
    test 36 (sexpr_eq (execute_read_sexpr "`a") (execute_expected(Pair(Symbol("quasiquote"),Pair(Symbol("a"),Nil)))));
    test 37 (sexpr_eq (execute_read_sexpr "`aaaa") (execute_expected(Pair(Symbol("quasiquote"),Pair(Symbol("aaaa"),Nil)))));
    test 38 (sexpr_eq (execute_read_sexpr "`bbbb") (execute_expected(Pair(Symbol("quasiquote"),Pair(Symbol("bbbb"),Nil)))));
    test 39 (sexpr_eq (execute_read_sexpr "`abcdef") (execute_expected(Pair(Symbol("quasiquote"),Pair(Symbol("abcdef"),Nil)))));
    test 40 (sexpr_eq (execute_read_sexpr "`abcdefghijklmnop") (execute_expected(Pair(Symbol("quasiquote"),Pair(Symbol("abcdefghijklmnop"),Nil)))));
    test 41 (sexpr_eq (execute_read_sexpr "`abcdefghijklmnopqrstuvwxyz") (execute_expected(Pair(Symbol("quasiquote"),Pair((Symbol("abcdefghijklmnopqrstuvwxyz"),Nil))))));
    test 42 (sexpr_eq (execute_read_sexpr "`abcdefghijklmnopqrstuvwxyz0123456789") (execute_expected(Pair(Symbol("quasiquote"),Pair(Symbol("abcdefghijklmnopqrstuvwxyz0123456789"),Nil)))));
    test 43 (sexpr_eq (execute_read_sexpr "`!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456789") (execute_expected(Pair(Symbol("quasiquote"),Pair(Symbol("!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456789"),Nil)))));
    test 44 (sexpr_eq (execute_read_sexpr "`()") (execute_expected(Pair(Symbol("quasiquote"),Pair(Nil,Nil)))));
    test 45 (sexpr_eq (execute_read_sexpr "`( #t )") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Bool(true), Nil),Nil)))));
    test 46 (sexpr_eq (execute_read_sexpr "`( \"this\" )") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(String("this"),Nil),Nil)))));
    test 47 (sexpr_eq (execute_read_sexpr "`( 37392 )") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Number(Int(37392)),Nil),Nil)))));
    test 48 (sexpr_eq (execute_read_sexpr "`( 37392.39382 )") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Number(Float(37392.39382)),Nil),Nil)))));
    test 49 (sexpr_eq (execute_read_sexpr "`( #\\c )") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Char('c'), Nil),Nil)))));
    test 50 (sexpr_eq (execute_read_sexpr "`( #\\c 37392.39382 )") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Nil)),Nil)))));
    test 51 (sexpr_eq (execute_read_sexpr "`( #\\c 37392.39382 37392 )") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Nil))),Nil)))));
    test 52 (sexpr_eq (execute_read_sexpr "`( #\\c 37392.39382 37392 \"this\" )") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Nil)))),Nil)))));
    test 53 (sexpr_eq (execute_read_sexpr "`(#\\c 37392.39382 37392 \"this\" #t)") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Pair(Bool(true), Nil))))),Nil)))));
    test 54 (sexpr_eq (execute_read_sexpr "`(#t . #f)") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Bool(true),Bool(false)),Nil)))));
    test 55 (sexpr_eq (execute_read_sexpr "`(#t . asfsfdsfa)") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Bool(true),Symbol("asfsfdsfa")),Nil)))));
    test 58 (sexpr_eq (execute_read_sexpr "`( #\\c . 37392.39382 )") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Char('c'), Number(Float(37392.39382))),Nil)))));
    test 59 (sexpr_eq (execute_read_sexpr "`( #\\c 37392.39382 . 37392 )") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Number(Int(37392)))),Nil)))));
    test 60 (sexpr_eq (execute_read_sexpr "`( #\\c 37392.39382 37392 . \"this\" )") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), String("this")))),Nil)))));
    test 61 (sexpr_eq (execute_read_sexpr "`(#\\c 37392.39382 37392 \"this\" . #t)") (execute_expected(Pair(Symbol("quasiquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Bool(true))))),Nil)))));
    ;;

    let test_Unquoted_ = fun () ->
    current_test := "test_Unquoted_";
    test 1 (sexpr_eq (execute_read_sexpr ",#t") (execute_expected(Pair(Symbol("unquote"),Pair(Bool(true),Nil)))));
    test 2 (sexpr_eq (execute_read_sexpr ",#f") (execute_expected(Pair(Symbol("unquote"),Pair(Bool(false),Nil)))));
    test 3 (sexpr_eq (execute_read_sexpr ",#\\a") (execute_expected(Pair(Symbol("unquote"),Pair(Char('a'),Nil)))));
    test 4 (sexpr_eq (execute_read_sexpr ",#\\g") (execute_expected(Pair(Symbol("unquote"),Pair(Char('g'),Nil)))));
    test 5 (sexpr_eq (execute_read_sexpr ",#\\4") (execute_expected(Pair(Symbol("unquote"),Pair(Char('4'),Nil)))));
    test 6 (sexpr_eq (execute_read_sexpr ",#\\#") (execute_expected(Pair(Symbol("unquote"),Pair(Char('#'),Nil)))));
    test 7 (sexpr_eq (execute_read_sexpr ",#\\newline") (execute_expected(Pair(Symbol("unquote"),Pair(Char('\n'),Nil)))));
    test 8 (sexpr_eq (execute_read_sexpr ",#\\nul") (execute_expected(Pair(Symbol("unquote"),Pair(Char(Char.chr 0),Nil)))));
    test 9 (sexpr_eq (execute_read_sexpr ",#\\return") (execute_expected(Pair(Symbol("unquote"),Pair(Char('\r'),Nil)))));
    test 10 (sexpr_eq (execute_read_sexpr ",#\\space") (execute_expected(Pair(Symbol("unquote"),Pair(Char(' '),Nil)))));
    test 11 (sexpr_eq (execute_read_sexpr ",#\\tab") (execute_expected(Pair(Symbol("unquote"),Pair(Char('\t'),Nil)))));
    test 15 (sexpr_eq (execute_read_sexpr ",4") (execute_expected(Pair(Symbol("unquote"),Pair(Number(Int 4),Nil)))));
    test 16 (sexpr_eq (execute_read_sexpr ",10") (execute_expected(Pair(Symbol("unquote"),Pair(Number (Int 10),Nil)))));
    test 17 (sexpr_eq (execute_read_sexpr ",+10") (execute_expected(Pair(Symbol("unquote"),Pair(Number (Int 10),Nil)))));
    test 18 (sexpr_eq (execute_read_sexpr ",-10") (execute_expected(Pair(Symbol("unquote"),Pair(Number(Int(-10)),Nil)))));
    test 19 (sexpr_eq (execute_read_sexpr ",-34324324324324") (execute_expected(Pair(Symbol("unquote"),Pair(Number(Int(-34324324324324)),Nil)))));
    test 20 (sexpr_eq (execute_read_sexpr ",-10.99") (execute_expected(Pair(Symbol("unquote"),Pair(Number(Float(-10.99)),Nil)))));
    test 23 (sexpr_eq (execute_read_sexpr ",10.99") (execute_expected(Pair(Symbol("unquote"),Pair(Number(Float(10.99)),Nil)))));
    test 25 (sexpr_eq (execute_read_sexpr ",\"This is a string\"") (execute_expected(Pair(Symbol("unquote"),Pair(String("This is a string"),Nil)))));
    test 26 (sexpr_eq (execute_read_sexpr ",\"This is a string with \\\\ \"") (execute_expected(Pair(Symbol("unquote"),Pair(String("This is a string with \\ "),Nil)))));
    test 27 (sexpr_eq (execute_read_sexpr ",\"This is a string with \\\" \"") (execute_expected(Pair(Symbol("unquote"),Pair(String("This is a string with \" "),Nil)))));
    test 28 (sexpr_eq (execute_read_sexpr ",\"This is a string with \\t \"") (execute_expected(Pair(Symbol("unquote"),Pair(String("This is a string with \t "),Nil)))));
    test 29 (sexpr_eq (execute_read_sexpr ",\"This is a string with \\f \"") (execute_expected(Pair(Symbol("unquote"),Pair(String(Printf.sprintf "This is a string with %c " (Char.chr 12)),Nil)))));
    test 30 (sexpr_eq (execute_read_sexpr ",\"This is a string with \\r \"") (execute_expected(Pair(Symbol("unquote"),Pair(String("This is a string with \r "),Nil)))));
    test 31 (sexpr_eq (execute_read_sexpr ",\"This is a string with \\n \"") (execute_expected(Pair(Symbol("unquote"),Pair(String("This is a string with \n "),Nil)))));
    test 36 (sexpr_eq (execute_read_sexpr ",a") (execute_expected(Pair(Symbol("unquote"),Pair(Symbol("a"),Nil)))));
    test 37 (sexpr_eq (execute_read_sexpr ",aaaa") (execute_expected(Pair(Symbol("unquote"),Pair(Symbol("aaaa"),Nil)))));
    test 38 (sexpr_eq (execute_read_sexpr ",bbbb") (execute_expected(Pair(Symbol("unquote"),Pair(Symbol("bbbb"),Nil)))));
    test 39 (sexpr_eq (execute_read_sexpr ",abcdef") (execute_expected(Pair(Symbol("unquote"),Pair(Symbol("abcdef"),Nil)))));
    test 40 (sexpr_eq (execute_read_sexpr ",abcdefghijklmnop") (execute_expected(Pair(Symbol("unquote"),Pair(Symbol("abcdefghijklmnop"),Nil)))));
    test 41 (sexpr_eq (execute_read_sexpr ",abcdefghijklmnopqrstuvwxyz") (execute_expected(Pair(Symbol("unquote"),Pair((Symbol("abcdefghijklmnopqrstuvwxyz"),Nil))))));
    test 42 (sexpr_eq (execute_read_sexpr ",abcdefghijklmnopqrstuvwxyz0123456789") (execute_expected(Pair(Symbol("unquote"),Pair(Symbol("abcdefghijklmnopqrstuvwxyz0123456789"),Nil)))));
    test 43 (sexpr_eq (execute_read_sexpr ",!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456789") (execute_expected(Pair(Symbol("unquote"),Pair(Symbol("!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456789"),Nil)))));
    test 44 (sexpr_eq (execute_read_sexpr ",()") (execute_expected(Pair(Symbol("unquote"),Pair(Nil,Nil)))));
    test 45 (sexpr_eq (execute_read_sexpr ",( #t )") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Bool(true), Nil),Nil)))));
    test 46 (sexpr_eq (execute_read_sexpr ",( \"this\" )") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(String("this"),Nil),Nil)))));
    test 47 (sexpr_eq (execute_read_sexpr ",( 37392 )") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Number(Int(37392)),Nil),Nil)))));
    test 48 (sexpr_eq (execute_read_sexpr ",( 37392.39382 )") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Number(Float(37392.39382)),Nil),Nil)))));
    test 49 (sexpr_eq (execute_read_sexpr ",( #\\c )") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Char('c'), Nil),Nil)))));
    test 50 (sexpr_eq (execute_read_sexpr ",( #\\c 37392.39382 )") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Nil)),Nil)))));
    test 51 (sexpr_eq (execute_read_sexpr ",( #\\c 37392.39382 37392 )") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Nil))),Nil)))));
    test 52 (sexpr_eq (execute_read_sexpr ",( #\\c 37392.39382 37392 \"this\" )") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Nil)))),Nil)))));
    test 53 (sexpr_eq (execute_read_sexpr ",(#\\c 37392.39382 37392 \"this\" #t)") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Pair(Bool(true), Nil))))),Nil)))));
    test 54 (sexpr_eq (execute_read_sexpr ",(#t . #f)") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Bool(true),Bool(false)),Nil)))));
    test 55 (sexpr_eq (execute_read_sexpr ",(#t . asfsfdsfa)") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Bool(true),Symbol("asfsfdsfa")),Nil)))));
    test 58 (sexpr_eq (execute_read_sexpr ",( #\\c . 37392.39382 )") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Char('c'), Number(Float(37392.39382))),Nil)))));
    test 59 (sexpr_eq (execute_read_sexpr ",( #\\c 37392.39382 . 37392 )") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Number(Int(37392)))),Nil)))));
    test 60 (sexpr_eq (execute_read_sexpr ",( #\\c 37392.39382 37392 . \"this\" )") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), String("this")))),Nil)))));
    test 61 (sexpr_eq (execute_read_sexpr ",(#\\c 37392.39382 37392 \"this\" . #t)") (execute_expected(Pair(Symbol("unquote"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Bool(true))))),Nil)))));
    ;;

      let test_UnquotedSpliced_ = fun () ->
    current_test := "test_UnquotedSpliced_";
    test 1 (sexpr_eq (execute_read_sexpr ",@#t") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Bool(true),Nil)))));
    test 2 (sexpr_eq (execute_read_sexpr ",@#f") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Bool(false),Nil)))));
    test 3 (sexpr_eq (execute_read_sexpr ",@#\\a") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Char('a'),Nil)))));
    test 4 (sexpr_eq (execute_read_sexpr ",@#\\g") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Char('g'),Nil)))));
    test 5 (sexpr_eq (execute_read_sexpr ",@#\\4") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Char('4'),Nil)))));
    test 6 (sexpr_eq (execute_read_sexpr ",@#\\#") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Char('#'),Nil)))));
    test 7 (sexpr_eq (execute_read_sexpr ",@#\\newline") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Char('\n'),Nil)))));
    test 8 (sexpr_eq (execute_read_sexpr ",@#\\nul") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Char(Char.chr 0),Nil)))));
    test 9 (sexpr_eq (execute_read_sexpr ",@#\\return") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Char('\r'),Nil)))));
    test 10 (sexpr_eq (execute_read_sexpr ",@#\\space") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Char(' '),Nil)))));
    test 11 (sexpr_eq (execute_read_sexpr ",@#\\tab") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Char('\t'),Nil)))));
    test 15 (sexpr_eq (execute_read_sexpr ",@4") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Number(Int 4),Nil)))));
    test 16 (sexpr_eq (execute_read_sexpr ",@10") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Number (Int 10),Nil)))));
    test 17 (sexpr_eq (execute_read_sexpr ",@+10") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Number (Int 10),Nil)))));
    test 18 (sexpr_eq (execute_read_sexpr ",@-10") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Number(Int(-10)),Nil)))));
    test 19 (sexpr_eq (execute_read_sexpr ",@-34324324324324") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Number(Int(-34324324324324)),Nil)))));
    test 20 (sexpr_eq (execute_read_sexpr ",@-10.99") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Number(Float(-10.99)),Nil)))));
    test 23 (sexpr_eq (execute_read_sexpr ",@10.99") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Number(Float(10.99)),Nil)))));
    test 25 (sexpr_eq (execute_read_sexpr ",@\"This is a string\"") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(String("This is a string"),Nil)))));
    test 26 (sexpr_eq (execute_read_sexpr ",@\"This is a string with \\\\ \"") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(String("This is a string with \\ "),Nil)))));
    test 27 (sexpr_eq (execute_read_sexpr ",@\"This is a string with \\\" \"") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(String("This is a string with \" "),Nil)))));
    test 28 (sexpr_eq (execute_read_sexpr ",@\"This is a string with \\t \"") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(String("This is a string with \t "),Nil)))));
    test 29 (sexpr_eq (execute_read_sexpr ",@\"This is a string with \\f \"") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(String(Printf.sprintf "This is a string with %c " (Char.chr 12)),Nil)))));
    test 30 (sexpr_eq (execute_read_sexpr ",@\"This is a string with \\r \"") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(String("This is a string with \r "),Nil)))));
    test 31 (sexpr_eq (execute_read_sexpr ",@\"This is a string with \\n \"") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(String("This is a string with \n "),Nil)))));
    test 36 (sexpr_eq (execute_read_sexpr ",@a") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Symbol("a"),Nil)))));
    test 37 (sexpr_eq (execute_read_sexpr ",@aaaa") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Symbol("aaaa"),Nil)))));
    test 38 (sexpr_eq (execute_read_sexpr ",@bbbb") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Symbol("bbbb"),Nil)))));
    test 39 (sexpr_eq (execute_read_sexpr ",@abcdef") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Symbol("abcdef"),Nil)))));
    test 40 (sexpr_eq (execute_read_sexpr ",@abcdefghijklmnop") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Symbol("abcdefghijklmnop"),Nil)))));
    test 41 (sexpr_eq (execute_read_sexpr ",@abcdefghijklmnopqrstuvwxyz") (execute_expected(Pair(Symbol("unquote-splicing"),Pair((Symbol("abcdefghijklmnopqrstuvwxyz"),Nil))))));
    test 42 (sexpr_eq (execute_read_sexpr ",@abcdefghijklmnopqrstuvwxyz0123456789") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Symbol("abcdefghijklmnopqrstuvwxyz0123456789"),Nil)))));
    test 43 (sexpr_eq (execute_read_sexpr ",@!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456789") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Symbol("!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456789"),Nil)))));
    test 44 (sexpr_eq (execute_read_sexpr ",@()") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Nil,Nil)))));
    test 45 (sexpr_eq (execute_read_sexpr ",@( #t )") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Bool(true), Nil),Nil)))));
    test 46 (sexpr_eq (execute_read_sexpr ",@( \"this\" )") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(String("this"),Nil),Nil)))));
    test 47 (sexpr_eq (execute_read_sexpr ",@( 37392 )") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Number(Int(37392)),Nil),Nil)))));
    test 48 (sexpr_eq (execute_read_sexpr ",@( 37392.39382 )") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Number(Float(37392.39382)),Nil),Nil)))));
    test 49 (sexpr_eq (execute_read_sexpr ",@( #\\c )") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Char('c'), Nil),Nil)))));
    test 50 (sexpr_eq (execute_read_sexpr ",@( #\\c 37392.39382 )") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Nil)),Nil)))));
    test 51 (sexpr_eq (execute_read_sexpr ",@( #\\c 37392.39382 37392 )") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Nil))),Nil)))));
    test 52 (sexpr_eq (execute_read_sexpr ",@( #\\c 37392.39382 37392 \"this\" )") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Nil)))),Nil)))));
    test 53 (sexpr_eq (execute_read_sexpr ",@(#\\c 37392.39382 37392 \"this\" #t)") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Pair(Bool(true), Nil))))),Nil)))));
    test 54 (sexpr_eq (execute_read_sexpr ",@(#t . #f)") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Bool(true),Bool(false)),Nil)))));
    test 55 (sexpr_eq (execute_read_sexpr ",@(#t . asfsfdsfa)") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Bool(true),Symbol("asfsfdsfa")),Nil)))));
    test 58 (sexpr_eq (execute_read_sexpr ",@( #\\c . 37392.39382 )") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Char('c'), Number(Float(37392.39382))),Nil)))));
    test 59 (sexpr_eq (execute_read_sexpr ",@( #\\c 37392.39382 . 37392 )") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Number(Int(37392)))),Nil)))));
    test 60 (sexpr_eq (execute_read_sexpr ",@( #\\c 37392.39382 37392 . \"this\" )") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), String("this")))),Nil)))));
    test 61 (sexpr_eq (execute_read_sexpr ",@(#\\c 37392.39382 37392 \"this\" . #t)") (execute_expected(Pair(Symbol("unquote-splicing"),Pair(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Bool(true))))),Nil)))));
    ;;

let test_LineComment_ = fun () ->
    current_test := "test_LineComment_";
    test 1 (sexpr_eq (execute_read_sexpr "; sdfdkerjdfk 4594359ksmdvc fdskfs\n#t") (execute_expected(Bool true)));
    test 2 (sexpr_eq (execute_read_sexpr "#f;sadasujnxjzcnjsaudj ij49fdsf") (execute_expected(Bool false)));
    test 3 (sexpr_eq (execute_read_sexpr " #f") (execute_expected(Bool false)));
    test 4 (sexpr_eq (execute_read_sexpr ";asdi39isksdkmkdsfkdskf\n #t") (execute_expected(Bool true)));
    test 5 (sexpr_eq (execute_read_sexpr "             ;dsfdsfertidrmdkmvdkgdgdffdg\n #t ") (execute_expected(Bool true)));
    test 6 (sexpr_eq (execute_read_sexpr " #f       ;dfkdsfjdisf dkfmk43rtmsdkfmzcpc3-33#@%#$^$##@#@!# ") (execute_expected(Bool false)));
    test 7 (sexpr_eq (execute_read_sexpr ";sfkdsfklsdkf\n ;dsflnjdsfhsdjkf\n #\\a") (execute_expected(Char 'a')));
    test 8 (sexpr_eq (execute_read_sexpr "#\\g         ;dsflksdf;ds;fk;sdf\n;dsf;sdflkjsdklfjdsf\n") (execute_expected(Char 'g')));
    test 9 (sexpr_eq (execute_read_sexpr ";asdjasdlasdjhasdjk   sadlsadld;sadklsaldklasd\n#\\4") (execute_expected(Char '4')));
    test 10 (sexpr_eq (execute_read_sexpr "#\\#          ;sadl;sakdlkdsallasdljasd\n") (execute_expected(Char '#')));
    test 11 (sexpr_eq (execute_read_sexpr ";sadsjdasdjashdjsad\n#\\newline") (execute_expected(Char '\n')));
    test 12 (sexpr_eq (execute_read_sexpr "#\\nul ;sadkjasdkjas;asdjkaskdjkasdj\n;sdfksdfkldskf\n") (execute_expected(Char (Char.chr 0))));
    test 13 (sexpr_eq (execute_read_sexpr "               ;sdajksafdkjsdkf\n ;sdjfksajksdfkjdsf\n     #\\return") (execute_expected(Char '\r')));
    test 14 (sexpr_eq (execute_read_sexpr "#\\space   ;\n") (execute_expected(Char ' ')));
    test 15 (sexpr_eq (execute_read_sexpr ";\n#\\tab") (execute_expected(Char '\t')));
    test 19 (sexpr_eq (execute_read_sexpr "4               ;dsfdsfsdfdsf") (execute_expected(Number (Int 4))));
    test 20 (sexpr_eq (execute_read_sexpr "10              \n\n;dsfdsfdsfsdfdsf") (execute_expected(Number (Int 10))));
    test 21 (sexpr_eq (execute_read_sexpr "+10            ;") (execute_expected(Number (Int 10))));
    test 22 (sexpr_eq (execute_read_sexpr "-10") (execute_expected(Number (Int (-10)))));
    test 23 (sexpr_eq (execute_read_sexpr ";\n\n\n-34324324324324") (execute_expected(Number (Int (-34324324324324)))));
    test 24 (sexpr_eq (execute_read_sexpr "-10.99 ;sdsadasdsadasd\n\n") (execute_expected(Number (Float (-10.99)))));
    test 27 (sexpr_eq (execute_read_sexpr "\n\n\n\n\n10.99\n\n\n\n") (execute_expected(Number (Float (10.99)))));
    test 29 (sexpr_eq (execute_read_sexpr "\"This is a string\";sdsfsdfsd\n") (execute_expected(String "This is a string")));
    test 30 (sexpr_eq (execute_read_sexpr "\"This is a string with \\\\ \";sadsdsadsadasdsda\n   ") (execute_expected(String "This is a string with \\ ")));
    test 31 (sexpr_eq (execute_read_sexpr ";sadsadsadas\n\"This is a string with \\\" \"") ((execute_expected(String "This is a string with \" "))));
    test 32 (sexpr_eq (execute_read_sexpr "\"This is a string with \\t \"       ;sdsadsadsadasd") ((execute_expected(String "This is a string with \t "))));
    test 33 (sexpr_eq (execute_read_sexpr "\"This is a string with \\f \"    ;\n\n\n") ((execute_expected(String (Printf.sprintf "This is a string with %c " (Char.chr 12))))));
    test 34 (sexpr_eq (execute_read_sexpr "\"This is a string with \\r \";sdsasasadsadsdsada\n") ((execute_expected(String "This is a string with \r "))));
    test 35 (sexpr_eq (execute_read_sexpr "\"This is a string with \\n \"\n\n\n\n\n\n") ((execute_expected(String "This is a string with \n "))));
    test 40 (sexpr_eq (execute_read_sexpr "a ;sfdsdfsdf\n\n") (execute_expected(Symbol "a")));
    test 41 (sexpr_eq (execute_read_sexpr "\n\n\n                         aaaa\n") (execute_expected(Symbol "aaaa")));
    test 42 (sexpr_eq (execute_read_sexpr "bbbb ;thisIsNotBeParsed") (execute_expected(Symbol "bbbb")));
    test 43 (sexpr_eq (execute_read_sexpr "abcdef ;dssadsadsadas\n\n") (execute_expected(Symbol "abcdef")));
    test 44 (sexpr_eq (execute_read_sexpr ";sdsasasad\n\n abcdefghijklmnop") (execute_expected(Symbol "abcdefghijklmnop")));
    test 45 (sexpr_eq (execute_read_sexpr "abcdefghijklmnopqrstuvwxyz ;dsadsadsadsasad\n\n") (execute_expected(Symbol "abcdefghijklmnopqrstuvwxyz")));
    test 46 (sexpr_eq (execute_read_sexpr "abcdefghijklmnopqrstuvwxyz0123456789 ;dsasdsa\n\n") (execute_expected(Symbol "abcdefghijklmnopqrstuvwxyz0123456789")));
    test 47 (sexpr_eq (execute_read_sexpr ";\n\n\n\n\n;dfdsfdsf\n !$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456789") (execute_expected(Symbol "!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456789")));
    test 48 (sexpr_eq (execute_read_sexpr "()        ;fdsfdsfdsfds\n") (execute_expected(Nil)));
    test 49 (sexpr_eq (execute_read_sexpr ";sdsadasd\n ;dfdsfdsf\n ;sdfdsfdsf\n ;sdfdsdsfsdf\n ;'34324asd@##@$$^%$&%\n ( #t )") (execute_expected(Pair(Bool(true), Nil))));
    test 50 (sexpr_eq (execute_read_sexpr "( \"this\" ;dsfdsfdsfdsfsdfds\n )") (execute_expected(Pair(String("this"), Nil))));
    test 51 (sexpr_eq (execute_read_sexpr "( ;dfsfdsfdsfdsf\n 37392 ;dsfdsfdsfdse43#@$@#$\n )") (execute_expected(Pair(Number(Int(37392)), Nil))));
    test 52 (sexpr_eq (execute_read_sexpr ";sfdsf3#@#@$\n ( ;sdfsdfdsfdsfdsf\n 37392.39382 ;#$#@$\n ) ;dfsdfdsfsdfsd") (execute_expected(Pair(Number(Float(37392.39382)), Nil))));
    test 53 (sexpr_eq (execute_read_sexpr "( #\\c ;sdsadsadsadsa\n)") (execute_expected(Pair(Char('c'), Nil))));
    test 54 (sexpr_eq (execute_read_sexpr ";asdasdasdas\n ( #\\c 37392.39382 )") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Nil)))));
    test 55 (sexpr_eq (execute_read_sexpr "( #\\c 37392.39382 ;#@$#@#@$#@DFDSC%DFfgdgdf\n 37392 )") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Nil))))));
    test 56 (sexpr_eq (execute_read_sexpr "( #\\c 37392.39382 ;dsfds$#$%#%\n 37392 ;dsfds$#$%#%\n ;45435gfdfdg\n ;45435$%#%\n \"this\" )") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Nil)))))));
    test 57 (sexpr_eq (execute_read_sexpr "(#\\c 37392.39382 37392 ;dsfds$#$%#%\n \"this\" #t)") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Pair(Bool(true), Nil))))))));
    test 58 (sexpr_eq (execute_read_sexpr "(#t . ;34324dfssfgfdg\n #f)") (execute_expected(Pair(Bool(true),Bool(false)))));
    test 59 (sexpr_eq (execute_read_sexpr "(#t . asfsfdsfa)") (execute_expected(Pair(Bool(true),Symbol("asfsfdsfa")))));
    test 62 (sexpr_eq (execute_read_sexpr "( #\\c ;sfdsfdsf\n\n . 37392.39382 )") (execute_expected(Pair(Char('c'), Number(Float(37392.39382))))));
    test 63 (sexpr_eq (execute_read_sexpr "( #\\c 37392.39382 . 37392 )") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Number(Int(37392)))))));
    test 64 (sexpr_eq (execute_read_sexpr "( #\\c 37392.39382 37392 ;fsdfds#$#$#%$#\n . \"this\" )") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), String("this")))))));
    test 65 (sexpr_eq (execute_read_sexpr "(#\\c 37392.39382 37392 \"this\" . ;324324DSFDSFSD\n #t)") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Bool(true))))))));
    ;;
  
  let test_SexprComment_ = fun () ->
    current_test := "test_SexprComment_";
    test 1 (sexpr_eq (execute_read_sexpr "#;#f #t") (execute_expected(Bool true)));
    test 2 (sexpr_eq (execute_read_sexpr "#;#f #f") (execute_expected(Bool false)));
    test 3 (sexpr_eq (execute_read_sexpr "#; #t #f ") (execute_expected(Bool false)));
    test 4 (sexpr_eq (execute_read_sexpr "#;#;#; #t #F #F #T") (execute_expected(Bool true)));
    test 5 (sexpr_eq (execute_read_sexpr "#;#; 3432432 3.45345 #t ") (execute_expected(Bool true)));
    test 6 (sexpr_eq (execute_read_sexpr " #; \"DSDSDD\" ;DSFSDFSDDSF\n #f ") (execute_expected(Bool false)));
    test 7 (sexpr_eq (execute_read_sexpr "#; #\\h #\\a") (execute_expected(Char 'a')));
    test 10 (sexpr_eq (execute_read_sexpr "#; \"Thisdssadsadis a string\" \"This is a string\"") (execute_expected(String "This is a string")));
    test 11 (sexpr_eq (execute_read_sexpr "#; \"This is a str#; #; ;ing with \\\\ \" \"This is a string with \\\\ \"") (execute_expected(String "This is a string with \\ ")));
    test 12 (sexpr_eq (execute_read_sexpr "a") (execute_expected(Symbol "a")));
    test 13 (sexpr_eq (execute_read_sexpr "#; dssdcve3232 #; vbvcc4gfdgd aaaa") (execute_expected(Symbol "aaaa")));
    test 14 (sexpr_eq (execute_read_sexpr "( #; dfdsfdsf #t )") (execute_expected(Pair(Bool(true), Nil))));
    test 15 (sexpr_eq (execute_read_sexpr "( #; #; #; #; a b c d \"this\" )") (execute_expected(Pair(String("this"), Nil))));
    test 16 (sexpr_eq (execute_read_sexpr "( #; 342342 #; \"dfdsf\" 37392.39382 )") (execute_expected(Pair(Number(Float(37392.39382)), Nil))));
    test 17 (sexpr_eq (execute_read_sexpr "( #; #F #; #T #\\c )") (execute_expected(Pair(Char('c'), Nil))));
    test 18 (sexpr_eq (execute_read_sexpr "( #\\c #; #\\5 37392.39382 #; 343242 )") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Nil)))));
    test 21 (sexpr_eq (execute_read_sexpr "(#\\c 37392.39382 37392 #; \"thfdsdfdsdsfdsfs\" \"this\" ;dfdsfdsfdsf\n #t)") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Pair(Bool(true), Nil))))))));
    test 22 (sexpr_eq (execute_read_sexpr "( #\\c 37392.39382 . 37392 )") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Number(Int(37392)))))));
    test 23 (sexpr_eq (execute_read_sexpr "( #\\c 37392.39382 37392 ;fsdfds#$#$#%$#\n . #; 423423 #;24324 \"this\" )") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), String("this")))))));
    test 24 (sexpr_eq (execute_read_sexpr "(#\\c 37392.39382 37392 \"this\" . ;324324DSFDSFSD\n #; ;asdasdasd\n #F #t)") (execute_expected(Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Bool(true))))))));
    test 25 (sexpr_eq (execute_read_sexpr "(;dsadsadsadas\n)") (execute_expected(Nil)));
    ;;

let test_ReadSexpr_ = fun () ->
    current_test := "test_ReadSexpr_";
    test 1 (sexpr_eq (execute_read_sexpr "#f #T #t") (execute_expected(String("test failed"))));
    test 2 (sexpr_eq (execute_read_sexpr "#t") (execute_expected(Bool(true))));
    test 3 (sexpr_eq (execute_read_sexpr ";asasdasfasf\n #t ;ffdsfdsfdsf") (execute_expected(Bool(true))));
    test 4 (sexpr_eq (execute_read_sexpr ";asasdasfasf\n #t ;ffdsfdsfdsf\n #f") (execute_expected(String("test failed"))));
    test 5 (sexpr_eq (execute_read_sexpr ";asasdasfasf\n #t ;ffdsfdsfdsf\n #; #f") (execute_expected(Bool(true))));
    ;;

let test_ReadSexprs_ = fun () ->
    current_test := "test_ReadSexprs_";
    test 1 (sexpr_eq_as_list (execute_read_sexprs_as_list "#f #T #t") (execute_expected_as_list([Bool(false); Bool(true); Bool(true)])));
    test 2 (sexpr_eq_as_list (execute_read_sexprs_as_list "#f 37392.39382 \"this\"") (execute_expected_as_list([Bool(false); Number(Float(37392.39382)); String("this")])));
    test 3 (sexpr_eq_as_list (execute_read_sexprs_as_list "#f 37392.39382 \"this\" fdsdfdsedfgg4fdgfd") (execute_expected_as_list([Bool(false); Number(Float(37392.39382)); String("this"); Symbol("fdsdfdsedfgg4fdgfd")])));
    test 4 (sexpr_eq_as_list (execute_read_sexprs_as_list "#f 37392.39382 \"this\" ;sfdsfdsfdsf\n fdsdfdsedfgg4fdgfd (#\\c 37392.39382 37392 \"this\" . ;324324DSFDSFSD\n #; ;asdasdasd\n #F #t)") (execute_expected_as_list([Bool(false); Number(Float(37392.39382)); String("this"); Symbol("fdsdfdsedfgg4fdgfd"); Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Int(37392)), Pair(String("this"), Bool(true)))))])));
    test 7 (sexpr_eq_as_list (execute_read_sexprs_as_list "") (execute_expected_as_list([])));
    test 8 (sexpr_eq_as_list (execute_read_sexprs_as_list "#f#t") (execute_expected_as_list([Bool(false); Bool(true)])));
    test 9 (sexpr_eq_as_list (execute_read_sexprs_as_list "123\"123\"") (execute_expected_as_list([Number(Int(123)); String("123")])));
    test 10 (sexpr_eq_as_list (execute_read_sexprs_as_list "#f'a") (execute_expected_as_list([Bool(false);Pair(Symbol("quote"), Pair(Symbol("a"),Nil))])));
    test 11 (sexpr_eq_as_list (execute_read_sexprs_as_list "#f`a") (execute_expected_as_list([Bool(false);Pair(Symbol("quasiquote"), Pair(Symbol("a"),Nil))])));
    test 12 (sexpr_eq_as_list (execute_read_sexprs_as_list "#f,a") (execute_expected_as_list([Bool(false);Pair(Symbol("unquote"), Pair(Symbol("a"),Nil))])));
    test 13 (sexpr_eq_as_list (execute_read_sexprs_as_list "#f,@a") (execute_expected_as_list([Bool(false);Pair(Symbol("unquote-splicing"), Pair(Symbol("a"),Nil))])));
    test 14 (sexpr_eq_as_list (execute_read_sexprs_as_list "#f(1 . 1.0)") (execute_expected_as_list([Bool(false);Pair(Number(Int(1)), Number(Float(1.0)))])));
    test 15 (sexpr_eq_as_list (execute_read_sexprs_as_list "#f(1 . 1.0)12345") (execute_expected_as_list([Bool(false);Pair(Number(Int(1)), Number(Float(1.0))); Number(Int(12345))])));
    test 17 (sexpr_eq_as_list (execute_read_sexprs_as_list "#f(1 1.0)12345") (execute_expected_as_list([Bool(false);Pair(Number(Int(1)),Pair(Number(Float(1.0)),Nil)); Number(Int(12345))])));
    ;;

let test_SceintificNotation_ = fun () ->
    current_test := "test_SceintificNotation_";
    test 1 (sexpr_eq (execute_read_sexpr "1e1") (execute_expected(Number (Float 10.0))));
    test 2 (sexpr_eq (execute_read_sexpr "10.0E2") (execute_expected(Number (Float 1000.0))));
    test 3 (sexpr_eq (execute_read_sexpr "1E+1") (execute_expected(Number (Float 10.0))));
    test 4 (sexpr_eq (execute_read_sexpr "10e-1") (execute_expected(Number (Float((1.0))))));
    test 5 (sexpr_eq (execute_read_sexpr "3.14e+9") (execute_expected(Number (Float (3140000000.0)))));
    test 6 (sexpr_eq (execute_read_sexpr "+000000012.3E00000002") (execute_expected(Number (Float (1230.0)))));
    test 7 (sexpr_eq (execute_read_sexpr "-5.000000000e-2") (execute_expected(Number (Float(-0.05)))));
    test 8 (sexpr_eq (execute_read_sexpr "50E-4") (execute_expected(Number (Float(0.005)))));
    test 9 (float_sexpr_eq (execute_read_sexpr "3.1234E+2") (execute_expected(Number (Float (312.34)))));
    test 10 (sexpr_eq (execute_read_sexpr "-50.345e+2") (execute_expected(Number (Float (-5034.5)))));
    test 11 (sexpr_eq (execute_read_sexpr "0.123E4") (execute_expected(Number (Float (1230.0)))));
    test 12 (float_sexpr_eq (execute_read_sexpr "-0.4321E-4") (execute_expected(Number (Float (-0.00004321)))));
    test 13 (float_sexpr_eq (execute_read_sexpr "-10.4321E-4") (execute_expected(Number (Float (-0.00104321)))));
    test 14 (float_sexpr_eq (execute_read_sexpr "-0.4321E-4") (execute_expected(Number (Float (-0.00004321)))));
    ;;

let tests = test_Boolean_ :: test_Char_ :: test_Number_ :: test_String_ :: test_Symbol_ :: test_List_ :: test_DottedList_  :: test_Quoted_ :: test_QuasiQuoted_ :: test_Unquoted_:: test_UnquotedSpliced_ :: test_LineComment_ :: test_SexprComment_ :: test_ReadSexpr_ :: test_ReadSexprs_ :: test_SceintificNotation_ :: [];;

let rec start_tests = function 
    | curr_test :: [] -> (try curr_test () with exn -> Printf.printf "%s%s Failed with %s Exception%s\n" red !current_test (Printexc.to_string exn) reset)
    | curr_test :: next_tests -> (try curr_test () with exn -> Printf.printf "%s%s Failed with %s Exception%s\n" red !current_test (Printexc.to_string exn) reset); start_tests next_tests
    | _ -> raise (Fatal_error "unexpected problom in start_tests");;

start_prompt ();;

start_tests tests;;

end_prompt ();;








#use "pc.ml";;

exception X_not_yet_implemented;;

exception X_this_should_not_happen;;

type number =
  | Int of int
  | Float of float;;

type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr
  | TaggedSexpr of string * sexpr
  | TagRef of string;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool b1, Bool b2 -> b1 = b2
  | Nil, Nil -> true
  | Number (Float f1), Number (Float f2) -> abs_float (f1 -. f2) < 0.001
  | Number (Int n1), Number (Int n2) -> n1 = n2
  | Char c1, Char c2 -> c1 = c2
  | String s1, String s2 -> s1 = s2
  | Symbol s1, Symbol s2 -> s1 = s2
  | Pair (car1, cdr1), Pair (car2, cdr2) -> sexpr_eq car1 car2 && sexpr_eq cdr1 cdr2
  | TaggedSexpr (name1, expr1), TaggedSexpr (name2, expr2) -> name1 = name2 && sexpr_eq expr1 expr2
  | TagRef name1, TagRef name2 -> name1 = name2
  | _ -> false;;

module Reader(*: sig
               val read_sexpr : string -> sexpr
               val read_sexprs : string -> sexpr list
               end *)=
struct
  let normalize_scheme_symbol str =
    if andmap (fun ch -> ch = lowercase_ascii ch) (string_to_list str)
    then str
    else Printf.sprintf "|%s|" str;;

  let _Bool_ = PC.disj (PC.pack (PC.word_ci "#f") (fun _ -> Bool false)) (PC.pack (PC.word_ci "#t") (fun _ -> Bool true));;

  let _CharPrefix_ = PC.word "#\\";;

  let _VisibleSimpleChar_ = PC.range_ci '!' '~';;
  let _NamedChar_ = PC.disj_list [PC.pack (PC.word_ci "nul") (fun _ -> '\000');
                                  PC.pack (PC.word_ci "newline") (fun _ -> '\n');
                                  PC.pack (PC.word_ci "return") (fun _ -> '\r');
                                  PC.pack (PC.word_ci "tab") (fun _ -> '\t');
                                  PC.pack (PC.word_ci "page") (fun _ -> '\012');
                                  PC.pack (PC.word_ci "space") (fun _ -> ' ')];;
  let _Char_ = PC.pack (PC.caten _CharPrefix_ (PC.disj _NamedChar_ _VisibleSimpleChar_)) (fun (_, ch) -> Char ch);;

  (* let _Digit_ = PC.pack _DigitChar_ (fun s -> int_of_char s - int_of_char '0');; *)

  let _DigitChar_ = PC.range '0' '9';;
  let _Natural_ = PC.pack (PC.plus _DigitChar_) (fun s -> int_of_string (list_to_string s));;
  let _PositiveInteger_ = PC.pack (PC.caten (PC.maybe (PC.char '+')) _Natural_) (fun (_, s) -> s);;
  let _NegativeInteger_ = PC.pack (PC.caten (PC.char '-') _Natural_) (fun (_, s) -> s * (-1));;
  let _Integer_ = (PC.disj_list [_NegativeInteger_; _PositiveInteger_]);;

  let _Float_ = PC.pack (PC.caten _Integer_ (PC.caten (PC.char '.') _Natural_))
      (fun (a, (_, s)) -> (float_of_string (string_of_int a ^ "." ^ string_of_int s)));;

  let _int_ = PC.pack _Integer_ (fun s -> Int s);;
  let _float_ = PC.pack _Float_ (fun s -> Float s);;
  let _ScientificNotation_ = PC.pack (PC.caten (PC.disj _float_ _int_) (PC.caten (PC.char_ci 'e') _Integer_))
      (fun (base, (_, exp)) ->
         match base with
         | Int b -> Float (float_of_int b *. (10.0 ** float_of_int exp))
         | Float f -> Float (f *. (10.0 ** float_of_int exp)));;
  let _Number_ = PC.pack (PC.disj_list [_ScientificNotation_; _float_; _int_]) (fun num -> Number num);;

  let _StringMetaChar_ = PC.disj_list [PC.pack (PC.word "\\\\") (fun _ -> "\\\\");
                                       PC.pack (PC.word "\\\"") (fun _ -> "\\\"");
                                       PC.pack (PC.word_ci "\\t") (fun _ -> "\\t");
                                       PC.pack (PC.word_ci "\\f") (fun _ -> "\\f");
                                       PC.pack (PC.word_ci "\\n") (fun _ -> "\\n");
                                       PC.pack (PC.word_ci "\\r") (fun _ -> "\\r")];;
  let _StringLiteralChar_ = PC.pack (PC.const (fun c -> (c!='"' && c!='\\'))) (fun c -> String.make 1 c);;
  let _StringChar_ = PC.pack (PC.disj _StringLiteralChar_ _StringMetaChar_)  (fun s -> String.get s 0);;
  let _String_ = PC.caten (PC.caten (PC.char '"') (PC.star _StringChar_)) (PC.char '"');;

  let _Symbol_ = PC.pack (PC.plus (PC.disj_list [_DigitChar_;
                                                 PC.range_ci 'a' 'z';
                                                 PC.char '!';
                                                 PC.char '$';
                                                 PC.char '^';
                                                 PC.char '*';
                                                 PC.char '-';
                                                 PC.char '_';
                                                 PC.char '=';
                                                 PC.char '+';
                                                 PC.char '<';
                                                 PC.char '>';
                                                 PC.char '?';
                                                 PC.char '/';
                                                 PC.char ':']
                                  )
                         )
      (fun s -> Symbol (list_to_string (List.map (fun c -> lowercase_ascii c) s)));;

  let _FoldPairList_ pairList = List.map (fun (se, _) -> se) pairList;;

  let _Nil_ = PC.pack (PC.word "()") (fun _ -> Nil);;

  let rec _Sexpr_ ss =
    let _disj_ = PC.disj_list [_Bool_; _Nil_; (*_Number_;*) _Char_; (*_String_;*) _Symbol_; _Quoted_; _QQuoted_; _UnquotedSpliced_; _Unquoted_ ; _List_; _DottedList_; _ListB_; _DottedListB_]
    in _disj_ ss

  and _List_ ss = PC.pack (PC.caten (PC.caten (PC.char '(') (PC.star PC.nt_whitespace)) (PC.caten (PC.plus (PC.caten _Sexpr_ (PC.star PC.nt_whitespace))) (PC.char ')')))
      (fun (_, (s, _)) -> List.fold_right (fun n1 n2 -> Pair (n1, n2)) (_FoldPairList_ s) Nil) ss

  and _DottedList_ ss = PC.pack (PC.caten (PC.caten (PC.char '(') (PC.star PC.nt_whitespace)) (PC.caten (PC.caten (PC.caten (PC.caten (PC.plus (PC.caten  _Sexpr_ (PC.star PC.nt_whitespace))) (PC.char '.')) (PC.star PC.nt_whitespace)) (PC.caten  _Sexpr_ (PC.star PC.nt_whitespace))) (PC.char ')')))
      (fun (_, (((((s, _), _), (e, _)), _))) -> List.fold_right (fun n1 n2 -> Pair (n1, n2)) (_FoldPairList_ s) e) ss

  and  _ListB_ ss = PC.pack (PC.caten (PC.caten (PC.char '[') (PC.star PC.nt_whitespace)) (PC.caten (PC.plus (PC.caten  _Sexpr_ (PC.star PC.nt_whitespace))) (PC.char ']')))
      (fun (_, (s, _)) -> List.fold_right (fun n1 n2 -> Pair (n1, n2)) (_FoldPairList_ s) Nil) ss

  and _DottedListB_ ss = PC.pack (PC.caten (PC.caten (PC.char '[') (PC.star PC.nt_whitespace)) (PC.caten (PC.caten (PC.caten (PC.caten (PC.plus (PC.caten  _Sexpr_ (PC.star PC.nt_whitespace))) (PC.char '.')) (PC.star PC.nt_whitespace)) (PC.caten  _Sexpr_ (PC.star PC.nt_whitespace))) (PC.char ']')))
      (fun (_, (((((s, _), _), (e, _)), _))) -> List.fold_right (fun n1 n2 -> Pair (n1, n2)) (_FoldPairList_ s) e) ss

  and _Quoted_ ss = PC.pack (PC.caten (PC.char '\'') _Sexpr_) (fun (_, s) -> Pair (Symbol "quote", Pair (s, Nil))) ss

  and _QQuoted_ ss = PC.pack (PC.caten (PC.char '`') _Sexpr_) (fun (_, s) -> Pair (Symbol "quasiquote", Pair (s, Nil))) ss

  and _UnquotedSpliced_ ss = PC.pack (PC.caten (PC.word ",@") _Sexpr_) (fun (_, s) -> Pair (Symbol "unquote-splicing", Pair (s, Nil))) ss

  and _Unquoted_ ss = PC.pack (PC.caten (PC.char ',') _Sexpr_) (fun (_, s) -> Pair (Symbol "unquote", Pair (s, Nil))) ss
  ;;

  let getSymbolvalue s = (*helper function to get the internal value of type*)
    match s with
    | Symbol s -> s
    | _ -> raise X_this_should_not_happen;;

  let _Tag_ = PC.pack (PC.caten (PC.word "#{") (PC.caten _Symbol_ (PC.word "}"))) (fun (_,(s,_)) -> TagRef (getSymbolvalue s));;
  let _TaggedExpr_ = PC.caten_list [(PC.word "#{"); (PC.pack _Symbol_ (fun s-> string_to_list (getSymbolvalue s))); (PC.word "}=")] ;;

  let read_sexpr string = raise X_not_yet_implemented;;

  let read_sexprs string = raise X_not_yet_implemented;;

end;; (* struct Reader *)

(*tests*)
PC.test_string Reader._int_ "-099";;
PC.test_string Reader._int_ "+099";;
PC.test_string Reader._float_ "-123.2";;
PC.test_string Reader._float_ "+123.2";;
PC.test_string Reader._float_ "3.14";;
PC.test_string Reader._Number_ "3.14";;
PC.test_string Reader._Number_ "+3.14";;
PC.test_string Reader._Number_ "3";;
PC.test_string Reader._Number_ "+3";;
PC.test_string Reader._Number_ "-3";;
PC.test_string Reader._Number_ "3.14e+9";;
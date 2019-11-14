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

  let _Bool_ =
    let _false_ = PC.pack (PC.word_ci "#f") (fun _ -> Bool false) in
    let _true_ = PC.pack (PC.word_ci "#t") (fun _ -> Bool true) in
    PC.disj _false_ _true_;;

  let _CharPrefix_ = PC.word "#\\";;

  let _VisibleSimpleChar_ = PC.range_ci '!' '~';;
  let _DigitChar_ = PC.range '0' '9';;
  let _af_ = PC.range_ci 'a' 'f'
  let _HexDigitChar_ = PC.disj _DigitChar_ _af_;;
  let _VisibleChar_ = PC.pack _VisibleSimpleChar_ (fun s -> Char s);;

  let _NamedChar_ =
    PC.disj_list [PC.pack (PC.word_ci "nul") (fun _ -> Char '\000');
                  PC.pack (PC.word_ci "newline") (fun _ -> Char '\n');
                  PC.pack (PC.word_ci "return") (fun _ -> Char '\r');
                  PC.pack (PC.word_ci "tab") (fun _ -> Char '\t');
                  PC.pack (PC.word_ci "page") (fun _ -> Char '\012');
                  PC.pack (PC.word_ci "space") (fun _ -> Char ' ')];;
  (*for example: #\spaceship    it's an error
    maybe the function that will call _NamedChar_ will check if the second element in pair is epsilon*)
  let _Char_ = PC.pack (PC.caten _CharPrefix_ (PC.disj _NamedChar_ _VisibleChar_)) (fun (_, ch) -> ch);;

  let _CheckedChar_ s =
    let (_, e) as result = _Char_ s in (*??????????*)
    if e=[]
    then result
    else raise X_this_should_not_happen;;

  let _Digit_ = PC.pack _DigitChar_ (fun s -> int_of_char s - (int_of_char '0'));;

  let _Natural_ = PC.pack (PC.plus _Digit_) (fun s -> List.fold_left (fun a b -> 10 * a + b) 0 s);;

  let _PositiveInteger_ = PC.pack (PC.caten (PC.char '+') _Natural_) (fun (_, s) -> s);;

  let _NegativeInteger_ = PC.pack (PC.caten (PC.char '-') _Natural_) (fun (_, s) -> s * (-1));;

  let _Integer_ = PC.disj_list [_NegativeInteger_ ; _PositiveInteger_ ; _Natural_];;

  let _Float_ = PC.pack (PC.caten ( PC.caten _Integer_ (PC.char '.')) _Natural_) (fun ((integer, _), nat) -> Float (float_of_string (string_of_int integer ^ "." ^ string_of_int nat)));;

  let _StringMetaChar_ =
    PC.disj_list [PC.pack (PC.word_ci "\r") (fun _ -> String "\r");
                  PC.pack (PC.word_ci "\n") (fun _ -> String "\n");
                  PC.pack (PC.word_ci "\t") (fun _ -> String "\t");
                  PC.pack (PC.word_ci "\f") (fun _ -> String "\012");
                  PC.pack (PC.word_ci "\\") (fun _ -> String "\092");
                  PC.pack (PC.word_ci "\"") (fun _ -> String "\034")];;
  (* let _StringLiteralChar_ = ;;
  let _StringChar_ = PC.disj _StringLiteralChar_ _StringMetaChar_;;
  let _String_ = PC.caten_list (PC.char '"') (star _StringChar_) (PC.char '"');;*)

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
    let _disj_ = PC.disj_list [_Bool_; _Nil_; (*_Number_;*) _Char_; (*_String_;*) _Symbol_; _Quoted_; _QQuoted_; _UnquotedSpliced_; _Unquoted_ ; (*_Vector_;*) _List_; _DottedList_; _ListB_; _DottedListB_] in
    _disj_ ss

  and _List_ ss = PC.pack (PC.caten (PC.caten (PC.char '(') (PC.star PC.nt_whitespace)) (PC.caten (PC.plus (PC.caten _Sexpr_ (PC.star PC.nt_whitespace))) (PC.char ')')))
      (fun (_, (s, _)) -> List.fold_right (fun n1 n2 -> Pair (n1, n2)) (_FoldPairList_ s) Nil) ss

  and _DottedList_ ss = PC.pack (PC.caten (PC.caten (PC.char '(') (PC.star PC.nt_whitespace)) (PC.caten (PC.caten (PC.caten (PC.caten (PC.plus (PC.caten  _Sexpr_ (PC.star PC.nt_whitespace))) (PC.char '.')) (PC.star PC.nt_whitespace)) (PC.caten  _Sexpr_ (PC.star PC.nt_whitespace))) (PC.char ')')))
      (fun (_, (((((s, _), _), (e, _)), _))) -> List.fold_right (fun n1 n2 -> Pair (n1, n2)) (_FoldPairList_ s) e) ss

  (* let _Vector_ ss = pack (caten (caten (word "#(") (star nt_whitespace)) (caten (plus (caten  _Sexpr_ (star nt_whitespace))) (char ')')))
      (fun (_, (s, _)) -> Vector (_FoldPairList_ s)) ss *)

  and  _ListB_ ss = PC.pack (PC.caten (PC.caten (PC.char '[') (PC.star PC.nt_whitespace)) (PC.caten (PC.plus (PC.caten  _Sexpr_ (PC.star PC.nt_whitespace))) (PC.char ']')))
      (fun (_, (s, _)) -> List.fold_right (fun n1 n2 -> Pair (n1, n2)) (_FoldPairList_ s) Nil) ss

  and _DottedListB_ ss = PC.pack (PC.caten (PC.caten (PC.char '[') (PC.star PC.nt_whitespace)) (PC.caten (PC.caten (PC.caten (PC.caten (PC.plus (PC.caten  _Sexpr_ (PC.star PC.nt_whitespace))) (PC.char '.')) (PC.star PC.nt_whitespace)) (PC.caten  _Sexpr_ (PC.star PC.nt_whitespace))) (PC.char ']')))
      (fun (_, (((((s, _), _), (e, _)), _))) -> List.fold_right (fun n1 n2 -> Pair (n1, n2)) (_FoldPairList_ s) e) ss

  and _Quoted_ ss = PC.pack (PC.caten (PC.char '\'') _Sexpr_) (fun (_, s) -> Pair (Symbol "quote", Pair (s, Nil))) ss

  and _QQuoted_ ss = PC.pack (PC.caten (PC.char '`') _Sexpr_) (fun (_, s) -> Pair (Symbol "quasiquote", Pair (s, Nil))) ss

  and _UnquotedSpliced_ ss = PC.pack (PC.caten (PC.word ",@") _Sexpr_) (fun (_, s) -> Pair (Symbol "unquote-splicing", Pair (s, Nil))) ss

  and _Unquoted_ ss = PC.pack (PC.caten (PC.char ',') _Sexpr_) (fun (_, s) -> Pair (Symbol "unquote", Pair (s, Nil))) ss
  ;;

  let read_sexpr string = raise X_not_yet_implemented;;

  let read_sexprs string = raise X_not_yet_implemented;;

end;; (* struct Reader *)

(*tests*)
PC.test_string Reader._NegativeInteger_ "-099";;
PC.test_string Reader._PositiveInteger_ "+099";;
PC.test_string Reader._Float_ "123.2";;
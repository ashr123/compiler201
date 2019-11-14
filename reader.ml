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

  let _NamedChar_ = PC.disj_list [PC.pack (PC.word_ci "nul") (fun _ -> '\000');
                                  PC.pack (PC.word_ci "newline") (fun _ -> '\n');
                                  PC.pack (PC.word_ci "return") (fun _ -> '\r');
                                  PC.pack (PC.word_ci "tab") (fun _ -> '\t');
                                  PC.pack (PC.word_ci "page") (fun _ -> '\012');
                                  PC.pack (PC.word_ci "space") (fun _ -> ' ')];;

  let _Char_ = PC.pack (PC.caten _CharPrefix_ (PC.disj _NamedChar_ _VisibleSimpleChar_)) (fun (_, ch) -> Char ch);;

  let _Digit_ = PC.pack _DigitChar_ (fun s -> int_of_char s - (int_of_char '0'));;

  let _Natural_ = PC.pack (PC.plus _Digit_) (fun s -> List.fold_left (fun a b -> 10 * a + b) 0 s);;
  let _PositiveInteger_ = PC.pack (PC.caten (PC.char '+') _Natural_) (fun (_, s) -> s);;
  let _NegativeInteger_ = PC.pack (PC.caten (PC.char '-') _Natural_) (fun (_, s) -> s * (-1));;
  let _Integer_ = PC.disj_list [_NegativeInteger_ ; _PositiveInteger_ ; _Natural_];;

  let _HexDigit_ = PC.pack _HexDigitChar_ (fun s -> float_of_string ("0x" ^ String.make 1 s));;
  let _HexNatural_ = PC.pack (PC.plus  _HexDigitChar_ )(fun s -> int_of_string ("0x" ^ list_to_string s));;
  let _HexNumNegative_ = PC.pack (PC.caten (PC.char '#') (PC.caten (PC.char_ci 'x') (PC.caten (PC.char '-') _HexNatural_))) (fun (_, (_, (_, s))) -> (-1) * s);;
  let _HexNumPositive_ = PC.pack (PC.caten (PC.char '#') (PC.caten (PC.char_ci 'x') (PC.caten (PC.char '+') _HexNatural_))) (fun (_, (_, (_, s)))-> s );;
  let _HexNatural2_ = PC.pack (PC.caten (PC.char '#') (PC.caten (PC.char_ci 'x') _HexNatural_)) (fun ((_, (_, s)))-> s);;
  let _HexInteger_ = PC.disj_list [_HexNumNegative_ ; _HexNumPositive_ ; _HexNatural2_];;

  let _FloatSS_ = PC.pack (PC.caten (PC.char '-') (PC.caten _Integer_ (PC.caten (PC.char '.') _Natural_)))
      (fun (_,(a, (_, s))) -> -1.0 *. float_of_string (string_of_int a ^ "." ^ string_of_int s));;

  let _Float_ = PC.pack (PC.caten _Integer_ (PC.caten (PC.char '.') _Natural_))
      (fun (a, (_, s)) -> float_of_string (string_of_int a ^ "." ^ string_of_int s));;

  let _HexFloat_ = PC.pack (PC.caten (PC.word_ci "#x") (PC.caten (PC.plus _HexDigitChar_) (PC.caten (PC.char '.') (PC.plus _HexDigitChar_))))
      (fun (_,(a, (_, s))) -> float_of_string ("0x" ^ list_to_string a ^ "." ^ list_to_string s));;

  let _HexFloatPlus_ = PC.pack (PC.caten (PC.word_ci "#x+") (PC.caten (PC.plus _HexDigitChar_) (PC.caten (PC.char '.') (PC.plus _HexDigitChar_))))
      (fun (_,(a, (_, s))) -> float_of_string ("0x" ^ list_to_string a ^ "." ^ list_to_string s));;

  let _HexFloatMinus_ = PC.pack (PC.caten (PC.word_ci "#x-") (PC.caten (PC.plus _HexDigitChar_) (PC.caten (PC.char '.') (PC.plus _HexDigitChar_))))
      (fun (_,(a, (_, s))) -> -1.0 *. float_of_string ("0x" ^ list_to_string a ^ "." ^ list_to_string s));;

  let _int_ = PC.pack (PC.disj _HexInteger_ _Integer_) (fun s -> Int s);;

  let _float_ = PC.pack (PC.disj_list [_HexFloatPlus_; _HexFloatMinus_; _HexFloat_; _FloatSS_ ; _Float_]) (fun s -> Float s);;

  let _StringMetaChar_ = PC.disj_list [PC.pack (PC.word "\\\\") (fun _ -> "\\\\");
                                       PC.pack (PC.word "\\\"") (fun _ -> "\\\"");
                                       PC.pack (PC.word_ci "\\t") (fun _ -> "\\t");
                                       PC.pack (PC.word_ci "\\f") (fun _ -> "\\f");
                                       PC.pack (PC.word_ci "\\n") (fun _ -> "\\n");
                                       PC.pack (PC.word_ci "\\r") (fun _ -> "\\r")];;
  let _StringLiteralChar_ = pack (PC.const (c!='"' && c!'\\')) (fun (c,_) ->  String.make 1 c);;
  let _StringChar_ = PC.disj _StringLiteralChar_ _StringMetaChar_;;
  let _String_ = PC.caten_list (PC.char '"') (star _StringChar_) (PC.char '"');;

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

  and _Unquoted_ ss = PC.pack (PC.caten (PC.char ',') _Sexpr_) (fun (_, s) -> Pair (Symbol "unquote", Pair (s, Nil))) ss;;

  let read_sexpr string = raise X_not_yet_implemented;;

  let read_sexprs string = raise X_not_yet_implemented;;

end;; (* struct Reader *)

(*tests*)
PC.test_string Reader._int_ "-099";;
PC.test_string Reader._int_ "+099";;
PC.test_string Reader._float_ "123.2";;
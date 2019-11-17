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

module Reader(* : sig
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

  let _CharCi_ = PC.range_ci 'a' 'z'
  and _DigitChar_ = PC.range '0' '9';;
  let _Natural_ = PC.pack (PC.plus _DigitChar_) (fun s -> int_of_string (list_to_string s));;
  let _PositiveInteger_ = PC.pack (PC.caten (PC.maybe (PC.char '+')) _Natural_) (fun (_, s) -> s);;
  let _NegativeInteger_ = PC.pack (PC.caten (PC.char '-') _Natural_) (fun (_, s) -> s * (-1));;
  let _Integer_ = (PC.disj_list [_NegativeInteger_; _PositiveInteger_]);;

  let _Float_ = PC.pack (PC.caten _Integer_ (PC.caten (PC.char '.') _Natural_))
      (fun (a, (_, s)) -> (float_of_string (string_of_int a ^ "." ^ string_of_int s)));;

  let radixNotation s =
    let num_of_char (ch : char) =
      let lowcaseNum = int_of_char (lowercase_ascii ch)
      in
      if lowcaseNum > int_of_char '9'
      then lowcaseNum - int_of_char 'a' + 10
      else lowcaseNum - int_of_char '0'
    and radixRange = PC.plus (PC.disj _CharCi_ _DigitChar_)
    in
    let floatingPoint n lst = List.fold_right (fun a b ->
        let num = num_of_char a
        in
        if num > n
        then raise PC.X_no_match
        else (float_of_int num +. b) /. float_of_int n) lst 0.0
    and natural n lst = List.fold_left (fun a b ->
        let num = num_of_char b
        in
        if num > n
        then raise PC.X_no_match
        else n * a + num) 0 lst
    in
    let generalFloatNTPlus n = PC.pack (PC.caten (PC.caten (PC.maybe (PC.char '+')) radixRange) (PC.caten (PC.char '.') radixRange))
        (fun ((_, a), (_, s)) -> float_of_int (natural n a) +. floatingPoint n s)
    and generalFloatNTMinus n = PC.pack (PC.caten (PC.caten (PC.char '-') radixRange) (PC.caten (PC.char '.') radixRange))
        (fun ((_, a), (_, s)) -> (float_of_int (natural n a) +. floatingPoint n s) *. -1.0)
    and generalPositiveInteger n = PC.pack (PC.caten (PC.maybe (PC.char '+')) radixRange) (fun (_, s) -> natural n s)
    and generalNegativeInteger n = PC.pack (PC.caten (PC.char '-') radixRange) (fun (_, s) -> natural n s * (-1))
    in
    let generalFloat n = PC.pack (PC.disj (generalFloatNTMinus n) (generalFloatNTPlus n)) (fun f -> Float f)
    and generalInteger n = PC.pack (PC.disj (generalNegativeInteger n) (generalPositiveInteger n)) (fun i -> Int i)
    in
    let ((n, _), s) = PC.caten (PC.guard _Natural_ (fun num -> 1 < num && num < 37)) (PC.char_ci 'r') s
    in
    PC.disj (generalFloat n) (generalInteger n) s
  ;;


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
  (* let _StringLiteralChar_ = PC.pack (PC.pack (fun s -> s) (fun c -> (c!='"' && c!='\\'))) (fun c -> String.make 1 c);;
     let _StringChar_ = PC.pack (PC.disj _StringLiteralChar_ _StringMetaChar_)  (fun s -> String.get s 0);;
     let _String_ = PC.caten (PC.caten (PC.char '"') (PC.star _StringChar_)) (PC.char '"');; *)

  let _Symbol_ = PC.pack (PC.plus (PC.disj_list [_DigitChar_;
                                                 _CharCi_;
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
    let _disj_ = PC.disj_list [_Bool_; _Nil_; _Number_; _Char_; (*_String_;*) _Symbol_; _Quoted_; _QQuoted_; _UnquotedSpliced_; _Unquoted_ ; _List_; _DottedList_; _ListB_; _DottedListB_]
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
  
  (*the comment is until end of line is reached or end of input*)
  let _LineComment_ = PC.pack (PC.caten_list [(PC.char ';'); PC.pack (PC.star (fun s -> PC.const (fun c -> (c!='\n')) s)) (fun s->' ');
                                    (PC.disj (PC.char '\n') (PC.pack PC.nt_end_of_input (fun s-> ' ')))])
                              (fun s->[]);;
  let _WhiteSpace_= PC.star (PC.char ' ');;

  let read_sexpr string = raise X_not_yet_implemented;;

  let read_sexprs string = raise X_not_yet_implemented;;

end;; (* struct Reader *)

(*#use "reader.ml";;*)
(*tests*)
PC.test_string Reader._Number_ "1e1";;
PC.test_string Reader._Number_ "1E+1";;
PC.test_string Reader._Number_ "10e-1";;
PC.test_string Reader._Number_ "3.14e+9";;
PC.test_string Reader._Number_ "3.14E-512";;
PC.test_string Reader._Number_ "+000000012.3E00000002";;
PC.test_string Reader._Number_ "-5.000000000e-2";;
PC.test_string Reader._Number_ "3.14";;
PC.test_string Reader._Number_ "+3.14";;
PC.test_string Reader._Number_ "3";;
PC.test_string Reader._Number_ "+3";;
PC.test_string Reader._Number_ "-3";;
PC.test_string Reader._Number_ "3.14e+9";;

PC.test_string Reader._LineComment_ ";Nadav is the king\n";;
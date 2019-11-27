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

module Reader : sig
  val read_sexpr : string -> sexpr
  val read_sexprs : string -> sexpr list
end =
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
  let _Integer_ = PC.disj _NegativeInteger_ _PositiveInteger_;;
  let _FloatNegative_ = PC.pack (PC.caten (PC.caten (PC.char '-') _Natural_) (PC.caten (PC.char '.') _Natural_))
      (fun ((_, a), (_, s)) ->(float_of_string ("-" ^ string_of_int a ^ "." ^ string_of_int s)));;
  let _FloatPositive_ = PC.pack (PC.caten _PositiveInteger_ (PC.caten (PC.char '.') _Natural_))
      (fun (a, (_, s)) ->(float_of_string (string_of_int a ^ "." ^ string_of_int s)));;

  let _Float_ = PC.disj _FloatNegative_ _FloatPositive_;;

  let radixNotation s =
    let num_of_char ch =
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
        then raise X_this_should_not_happen
        else (float_of_int num +. b) /. float_of_int n) lst 0.0
    and natural n lst = List.fold_left (fun a b ->
        let num = num_of_char b
        in
        if num > n
        then raise X_this_should_not_happen
        else n * a + num) 0 lst
    in
    let generalFloatNTPlus n = PC.pack (PC.caten (PC.caten (PC.maybe (PC.char '+')) radixRange) (PC.caten (PC.char '.') radixRange))
        (fun ((_, a), (_, s)) -> float_of_int (natural n a) +. floatingPoint n s)
    and generalFloatNTMinus n = PC.pack (PC.caten (PC.caten (PC.char '-') radixRange) (PC.caten (PC.char '.') radixRange))
        (fun ((_, a), (_, s)) -> (float_of_int (natural n a) +. floatingPoint n s) *. -1.0)
    and generalPositiveInteger n = PC.pack (PC.caten (PC.maybe (PC.char '+')) radixRange) (fun (_, s) -> natural n s)
    and generalNegativeInteger n = PC.pack (PC.caten (PC.char '-') radixRange) (fun (_, s) -> natural n s * -1)
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
         let e = 10.0 ** float_of_int exp
         in
         Float (match base with
             | Int b -> float_of_int b *. e
             | Float f -> f *. e));;
  let _Number_ = PC.pack (PC.not_followed_by (PC.disj_list [_ScientificNotation_; radixNotation; _float_; _int_]) _CharCi_) (fun num -> Number num);;

  let _StringMetaChar_ = PC.disj_list [PC.pack (PC.word "\\\\") (fun _ -> "\\");
                                       PC.pack (PC.word "\\\"") (fun _ -> "\"");
                                       PC.pack (PC.word_ci "\\t") (fun _ -> "\t");
                                       PC.pack (PC.word_ci "\\f") (fun _ -> "\012");
                                       PC.pack (PC.word_ci "\\n") (fun _ -> "\n");
                                       PC.pack (PC.word_ci "\\r") (fun _ -> "\r")];;
  let _StringLiteralChar_ = PC.pack (fun s -> PC.const (fun c -> (c <> '"' && c <> '\\')) s) (fun c -> String.make 1 c);;
  let _StringChar_ = PC.pack (PC.disj _StringLiteralChar_ _StringMetaChar_)  (fun s -> String.get s 0);;
  let _String_ = PC.pack (PC.caten (PC.caten (PC.char '"') (PC.star _StringChar_)) (PC.char '"')) (fun ((_, s), _) -> String (list_to_string s));;

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
 let _Number_ = PC.diff _Number_ _Symbol_;;*)

  let makeWrapped ntleft ntright nt = PC.pack (PC.caten (PC.caten ntleft nt) ntright) (fun ((_, e), _) -> e);;
  let _LineComment_ = PC.pack (PC.caten (PC.caten (PC.char ';') (PC.star (PC.const (fun c -> c <> '\n'))))
                                 (PC.disj (PC.char '\n') (PC.pack (PC.nt_end_of_input) (fun _ -> ' '))))
      (fun _ -> Nil);;   (*returns s-expression bc it's ignored in read_sexprs*)
  let _WhiteSpaces_ = PC.pack (PC.plus PC.nt_whitespace) (fun _ -> Nil);;   (*same here, ignored in read_sexprs*)

  let getSymbolvalue = (*helper function to get the internal value of type*)
    function
    | Symbol s -> s
    | _ -> raise X_this_should_not_happen
  ;;
  let _TagRef_ ss= (*Printf.printf "tag ref: %s\n" (list_to_string ss); *)
    PC.pack (PC.caten (PC.word "#{") (PC.caten _Symbol_ (PC.char '}'))) (fun (_, (s, _)) -> TagRef (getSymbolvalue s)) ss;;

  let rec _Sexpr_ ss=
    let _disj_ = PC.disj_list [_Bool_; _Number_; _Char_; _String_; _Symbol_; _Quoted_; _QQuoted_; _UnquotedSpliced_; _Unquoted_ ; _List_; _DottedList_; _TaggedExpr_; _TagRef_ (*the order of the last 2 is important*)]
    in
    makeWrapped _Skip_ _Skip_ _disj_ ss

  and _SexpComment_ ss = PC.pack (PC.caten (PC.word "#;") _Sexpr_) (fun _ -> Nil) ss
  and _Comment_ ss = PC.disj _LineComment_ _SexpComment_ ss
  and _Skip_ ss = PC.star (PC.disj _Comment_ _WhiteSpaces_) ss
  and _LeftParen_ ss = makeWrapped _Skip_ _Skip_ (PC.char '(') ss
  and _RightParen_ ss = makeWrapped _Skip_ _Skip_ (PC.char ')') ss

  and tocheck ss = (* Printf.printf "tocheck: %s\n" (list_to_string ss); *)
    PC.caten (PC.word "#{") (PC.caten (PC.pack _Symbol_ (fun s -> string_to_list (getSymbolvalue s))) (PC.caten (PC.word "}=") _Sexpr_)) ss
  and _TaggedExpr_ ss =(* Printf.printf "taggedA: %s\n" (list_to_string ss); *)
    PC.pack tocheck (fun (_, (string, (_, sexpr))) -> TaggedSexpr (list_to_string string, sexpr)) ss

  and _List_ ss = PC.pack (PC.caten _LeftParen_ (PC.caten (PC.star _Sexpr_) _RightParen_))
      (fun (_, (s, _)) -> List.fold_right (fun n1 n2 -> Pair (n1, n2)) s Nil) ss

  and _DottedList_ ss = PC.pack (PC.caten _LeftParen_ (PC.caten (PC.plus _Sexpr_) (PC.caten (PC.char '.') (PC.caten _Sexpr_ _RightParen_))))
      (fun (_, (s, (_, (e, _)))) -> List.fold_right (fun n1 n2 -> Pair (n1, n2)) s e) ss

  (*in the next cases wrapping the special chars with _Skip_ is not needed
    the possible _Skip_s are: in the begining, between the char and the _Sexpr_, at the end
    all are caught in _Sexpr_*)
  and _Quoted_ ss = PC.pack (PC.caten (PC.char '\'') _Sexpr_) (fun (_, s) -> Pair (Symbol "quote", Pair (s, Nil))) ss

  and _QQuoted_ ss = PC.pack (PC.caten (PC.char '`') _Sexpr_) (fun (_, s) -> Pair (Symbol "quasiquote", Pair (s, Nil))) ss

  and _UnquotedSpliced_ ss = PC.pack (PC.caten (PC.word ",@") _Sexpr_) (fun (_, s) -> Pair (Symbol "unquote-splicing", Pair (s, Nil))) ss

  and _Unquoted_ ss = PC.pack (PC.caten (PC.char ',') _Sexpr_) (fun (_, s) -> Pair (Symbol "unquote", Pair (s, Nil))) ss
  ;;

  (*s-expression with whitespaces* before&after, and maybe comment in the end, ((_,s),(_,_))*)
  (*coners all options: at first, we have comment (ends with '\n'),or whitespaces, than Sexpr, than comment maybe *)
  (*(PC.disj _WhiteSpaces_ _LineComment_)  =====  (PC.caten _WhiteSpaces_ (PC.maybe _LineComment_)) *)

  let makeSkipped = makeWrapped _Skip_ _Skip_;;

  let check () =
    let tagNamesList = ref []
    in
    fun sexpr ->
      let rec check =
        function
        | Pair (car, cdr) -> check car && check cdr
        | TaggedSexpr (name, sexpr) ->
          if List.mem name !tagNamesList
          then
            false
          else
            (tagNamesList := name :: !tagNamesList;
             check sexpr)
        | _ -> true
      in
      check sexpr
  ;;

  let read_sexpr string = (*as sayed in forum, Nil will be returned only in "()", means everything not real Sexpr will raise exception
                            not S-expr: "" or "   " or only line comment*)
    let ((acc, _), _) = PC.caten (makeSkipped _Sexpr_) PC.nt_end_of_input (string_to_list string)
    in
    if check () acc
    then acc
    else raise X_this_should_not_happen;;

  let read_sexprs string = (*here everything is ok, and souldn't raise exception if it's legal, just return []*)
    let ((acc, _), _) = PC.caten (PC.star (makeSkipped _Sexpr_)) PC.nt_end_of_input (string_to_list string)
    in
    if andmap (fun sexpr -> check () sexpr) acc
    then acc
    else raise X_this_should_not_happen;;

end;; (* struct Reader *)

(*#use "reader.ml";;*)
(*tests*)

(*PC.test_string Reader._Number_ "1e1";;
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
  PC.test_string Reader._Number_ "36rZZ";;
  PC.test_string Reader._Number_ "16R11.8a";;
  PC.test_string Reader._Number_ "2R-1101";;
  PC.test_string Reader._Number_ "2R+1101";;
  PC.test_string Reader._Number_ "1.00";;
  PC.test_string Reader._LineComment_ ";Nadav is the king\n";;
  PC.test_string Reader._Sexpr_ "\"abc\"";;

  Reader.read_sexpr "1e1";;
  Reader.read_sexprs "1e1; this is a comment\n   #t";;
  Reader.read_sexpr "()";;
  Reader.read_sexpr "55f";;
  Reader.read_sexpr "3.14E-512";;
  Reader.read_sexpr "3.14E+9";;
  Reader.read_sexpr "2r-1101";;
  Reader.read_sexpr "2r+1101";;
  Reader.read_sexpr "16R11.8a";;

  Reader.read_sexprs "";;
  Reader.read_sexprs "1e1";;
  Reader.read_sexprs "1e1 ; this is a comment";;
  Reader.read_sexprs "; this is a comment";;
  Reader.read_sexprs "; this is a comment\n";;
  Reader.read_sexprs "()";;
  Reader.read_sexprs "    ";;
  Reader.read_sexprs "55f #t";;
  Reader.read_sexprs "3.14E-512";;
  Reader.read_sexprs "3.14E+9";;
  Reader.read_sexprs "2r-1101";;
  Reader.read_sexprs "2r+1101";;
  Reader.read_sexprs "16R11.8a";;

  Reader.read_sexprs "()";;
  Reader.read_sexprs " #;  1e1 #t";;
  Reader.read_sexprs "#f    #;  1e1 #t ;hi\n";;
  Reader.read_sexprs "#f         #; #; ; 1e1 #t";;
  Reader.read_sexpr "(;hi
  )";;
  Reader.read_sexprs "(;hi
  )";;
*)

(*Reader.read_sexpr "#{foo}=(1 2 3)";;
  Reader.read_sexprs "#{foo}=(1 2 3) (1 #{foo}=2 #{foo})";;
  Reader.read_sexpr "#{x}=(a. #{x})";;
  Reader.read_sexprs "#{x}=(a. #{x})";;*)

(*Exceptions of all tests
  Reader.read_sexpr "";;
  Reader.read_sexpr "    ";;
  Reader.read_sexpr "; this is a comment";;
  Reader.read_sexpr "; this is a comment\n";;
  Reader.read_sexpr "1#t";;
  Reader.read_sexpr "(#;)";;
  Reader.read_sexpr "#{foo}=(#{foo}=1 2 3)";;
  Reader.read_sexprs "#{foo}=(#{foo}=1 2 3)";;
  Reader.read_sexprs "#{foo}=(#{foo}=1 2 3)";;
  Reader.read_sexprs "#{foo}=(1 2 3) (1 #{foo}=2 #{foo})";;
  Reader.read_sexpr "#{foo}=(#{foo}=1 2 3)";;
*)

(*Roy please delete this!!!!!!!!!!!*)
(* let check () =
   let tagNamesList = ref []
   in
   fun sexpr ->
    tagNamesList := sexpr :: !tagNamesList;
    !tagNamesList
   ;;
   let check1 = check ()
   and check2 = check ();;
   check1 1;;
   check2 2;;
   check1 3;;
   check2 4;; *)

(*Reader.read_sexpr "-0.4321";;
  Reader.read_sexprs "#{foo}=(1 2 3) (1 #{foo}=2 #{foo})";;*)
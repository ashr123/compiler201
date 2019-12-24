#use "reader.ml";;
open Reader;;

type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * expr list;;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const (Sexpr s1), Const (Sexpr s2) -> sexpr_eq s1 s2
  | Var v1, Var v2 -> String.equal v1 v2
  | If (t1, th1, el1), If (t2, th2, el2) -> expr_eq t1 t2 &&
                                            expr_eq th1 th2 &&
                                            expr_eq el1 el2
  | Seq l1, Seq l2
  | Or l1, Or l2 -> List.for_all2 expr_eq l1 l2
  | Set (var1, val1), Set (var2, val2)
  | Def (var1, val1), Def (var2, val2) -> expr_eq var1 var2 &&
                                          expr_eq val1 val2
  | LambdaSimple (vars1, body1), LambdaSimple (vars2, body2) ->
    List.for_all2 String.equal vars1 vars2 &&
    expr_eq body1 body2
  | LambdaOpt (vars1, var1, body1), LambdaOpt (vars2, var2, body2) ->
    String.equal var1 var2 &&
    List.for_all2 String.equal vars1 vars2 &&
    expr_eq body1 body2
  | Applic (e1, args1), Applic (e2, args2) ->
    expr_eq e1 e2 &&
    List.for_all2 expr_eq args1 args2
  | _ -> false;;


exception X_syntax_error;;

module type TAG_PARSER =  sig
  val tag_parse_expression : sexpr -> expr
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

  let reserved_word_list =
    ["and"; "begin"; "cond"; "define"; "else"; "if"; "lambda"; "let"; "let*";
     "letrec"; "or"; "quasiquote"; "quote"; "set!"; "unquote"; "unquote-splicing"];;

  (* work on the tag parser starts here *)

  let rec tag_parse sexpr =
    match sexpr with
    | Pair (Symbol "cond", cond) -> parseCond cond
    | Pair (Symbol "and", args) -> parseAnd args
    | TaggedSexpr (name, Pair (Symbol "quote", Pair (data, Nil))) -> Const (Sexpr (TaggedSexpr (name, data)))
    | Pair (Symbol "if", Pair (test, Pair (dit, Nil))) -> If (tag_parse test, tag_parse dit, Const Void)
    | Pair (Symbol "if", Pair (test, Pair (dit, Pair (dif, Nil)))) -> If (tag_parse test, tag_parse dit, tag_parse dif)
    | Pair (Symbol "define", Pair (Pair (Symbol name, args), body)) -> tag_parse (Pair (Symbol "define", Pair (Symbol name, Pair (Pair (Symbol "lambda", Pair (args, body)), Nil))))
    | Pair (Symbol "define", Pair (Symbol name, Pair (sexpr, Nil))) -> Def (tag_parse (Symbol name), tag_parse sexpr)
    | Pair (Symbol "let", Pair (bindings , body)) -> tag_parse (Pair (Pair (Symbol "lambda", Pair (getArgs bindings, body)), getVals bindings))
    | Pair (Symbol "let*", Pair (bindings, body)) -> tag_parse (parseLetStar bindings body)
    | Pair (Symbol "letrec", Pair (bindings, body)) -> tag_parse (parseLetRec bindings body)
    | Pair (Symbol "set!", Pair (Symbol sym, Pair (arg, Nil))) -> Set (tag_parse (Symbol sym), tag_parse arg)
    | Pair (Symbol "begin", bodies) -> sequencesExpr bodies
    | Pair (Symbol "or", args) -> parseOr args
    | Pair (Symbol "quote", Pair (x, Nil)) -> Const (Sexpr x)
    | Pair (Symbol "lambda", Pair (args, bodies)) -> parseLambda args bodies
    | Pair (Symbol "quasiquote", Pair (x, Nil)) -> parseQuasiquote x
    | Pair (exp1, rest) -> Applic (tag_parse exp1, List.map tag_parse (pairToList rest))
    | Number _|Char _|Bool _|String _|TagRef _|TaggedSexpr _ -> Const (Sexpr sexpr)
    | Symbol s ->
      if List.mem s reserved_word_list
      then raise X_syntax_error
      else Var s
    | _ -> Const Void

  and parseAnd =
    function
    | Nil -> tag_parse (Bool true)
    | Pair (x, Nil) -> tag_parse x
    | Pair (x, nextArgs) -> If (tag_parse x, parseAnd nextArgs, tag_parse (Bool false))
    | _ -> raise X_syntax_error

  and parseCond =
    function
    (* Reader.read_sexpr
       "(let ((value expr)
              (f (lambda ()
                   expr_f)))
          (if value
              ((f) value)))";; *)
    | Pair (Pair (expr, Pair (Symbol "=>", Pair (expr_f, Nil))), Nil) -> tag_parse (Pair (Symbol "let",
                                                                                          Pair
                                                                                            (Pair (Pair (Symbol "value", Pair (expr, Nil)),
                                                                                                   Pair
                                                                                                     (Pair (Symbol "f",
                                                                                                            Pair (Pair (Symbol "lambda", Pair (Nil, Pair (expr_f, Nil))),
                                                                                                                  Nil)),
                                                                                                      Nil)),
                                                                                             Pair
                                                                                               (Pair (Symbol "if",
                                                                                                      Pair (Symbol "value",
                                                                                                            Pair (Pair (Pair (Symbol "f", Nil), Pair (Symbol "value", Nil)), Nil))),
                                                                                                Nil)))) (* case 2 if last *)
    (* Reader.read_sexpr
       "(let ((value expr)
              (f (lambda ()
                   expr_f))
              (rest (lambda ()
                      restCond)))
          (if value
              ((f) value)
              (rest)))";; *)
    | Pair (Pair (expr, Pair (Symbol "=>", Pair (expr_f, Nil))), nextCond) -> tag_parse (Pair (Symbol "let",
                                                                                               Pair
                                                                                                 (Pair (Pair (Symbol "value", Pair (expr, Nil)),
                                                                                                        Pair
                                                                                                          (Pair (Symbol "f",
                                                                                                                 Pair (Pair (Symbol "lambda", Pair (Nil, Pair (expr_f, Nil))),
                                                                                                                       Nil)),
                                                                                                           Pair
                                                                                                             (Pair (Symbol "rest",
                                                                                                                    Pair
                                                                                                                      (Pair (Symbol "lambda", Pair (Nil, Pair (Pair (Symbol "cond", nextCond), Nil))),
                                                                                                                       Nil)),
                                                                                                              Nil))),
                                                                                                  Pair
                                                                                                    (Pair (Symbol "if",
                                                                                                           Pair (Symbol "value",
                                                                                                                 Pair (Pair (Pair (Symbol "f", Nil), Pair (Symbol "value", Nil)),
                                                                                                                       Pair (Pair (Symbol "rest", Nil), Nil)))),
                                                                                                     Nil)))) (* case 2 *)
    | Pair (Pair (Symbol "else", then1), _) -> tag_parse (Pair (Symbol "begin", then1)) (* case 3 *)
    | Pair (Pair (cond1, then1), Nil) -> tag_parse (Pair (Symbol "if" ,Pair (cond1, Pair (Pair (Symbol "begin", then1), Nil)))) (* case 1, last cond *)
    | Pair (Pair (cond1, then1), nextCond) -> tag_parse (Pair (Symbol "if", Pair (cond1, Pair (Pair (Symbol "begin", then1), Pair (Pair (Symbol "cond", nextCond), Nil))))) (* case 1 rest *)
    | _ -> raise X_syntax_error

  and parseQuasiquote x =
    match x with
    | Pair (Symbol "unquote", Pair (exp, Nil)) -> tag_parse exp (* case 1 *)
    | Pair (Symbol "unquote-splicing", Pair (_, Nil)) -> raise X_syntax_error (* case 2 *)
    | Nil|Symbol _ -> tag_parse (Pair (Symbol "quote", Pair (x, Nil))) (* case 3 *)
    (* DO NOT DELETE!!! *)
    (* | Pair (Pair (Symbol "unquote-splicing", Pair (exp, Nil)), Nil) -> Applic (Var "append", [tag_parse exp; Const (Sexpr Nil)]) (* case 5a ????? *) *)
    | Pair (Pair (Symbol "unquote-splicing", Pair (exp_a, Nil)), exp_b) -> Applic (Var "append", [tag_parse exp_a; tag_parse (Pair (Symbol "quasiquote", Pair (exp_b, Nil)))]) (* case 5a *)
    | Pair (exp_a, Pair (Symbol "unquote-splicing", Pair (exp_b, Nil))) -> Applic (Var "cons", [tag_parse (Pair (Symbol "quasiquote", Pair (exp_a, Nil))); tag_parse exp_b]) (* case 5b *)
    | Pair (exp_a, exp_b) -> Applic (Var "cons", [tag_parse (Pair (Symbol "quasiquote", Pair (exp_a, Nil))); tag_parse (Pair (Symbol "quasiquote", Pair (exp_b, Nil)))]) (* case 5c *)
    (* | Vector lst -> Applic (Var "vector", List.map (fun x -> tag_parse (Pair (Symbol "quote", Pair (x, Nil)))) lst) (* case 4 *) *)
    | Number _|Bool _|String _|Char _|TagRef _|TaggedSexpr _ -> tag_parse x (* the rest *)

  and pairToList =
    function
    | Nil -> []
    | Pair (left, right) -> left :: pairToList right
    | _ -> raise X_syntax_error

  and parseOr args =
    match args with
    | Nil -> tag_parse (Bool false)
    | Pair (x, Nil) -> tag_parse x
    | _ -> Or (List.map tag_parse (pairToList args))

  and getArgs =
    function
    | Nil -> Nil
    (* | Pair (Pair (arg, v), Nil) -> Pair (arg, Nil) *)
    | Pair (Pair (arg, Pair (_, Nil)), bindings) -> Pair (arg, getArgs bindings)
    | _ -> raise X_syntax_error

  and getVals =
    function
    | Nil -> Nil
    (* | Pair (Pair (arg, Pair (v, Nil)), Nil) -> Pair (v, Nil) *)
    | Pair (Pair (_, Pair (v, Nil)), bindings) -> Pair (v, getVals bindings)
    | _ -> raise X_syntax_error

  and parseLetStar bindings body =
    match bindings with
    | Nil -> Pair (Symbol "let", Pair (bindings, body))
    | Pair (Pair (arg, Pair (v, Nil)), Nil) -> Pair (Symbol "let", Pair (Pair (Pair (arg, Pair (v, Nil)), Nil), body))
    | Pair (Pair (arg, Pair (v, Nil)), bindings) -> Pair (Symbol "let", Pair (Pair (Pair (arg, Pair (v, Nil)), Nil), Pair (parseLetStar bindings body, Nil)))
    | _ -> raise X_syntax_error

  and parseLetRec bindings body = Pair (Symbol "let", Pair (parseLetRecBindings bindings, parseLetRecBody bindings body))

  and parseLetRecBindings =
    function
    | Nil -> Nil
    (* | Pair (Pair (arg, Pair (v, Nil)), Nil) -> Pair (Pair (arg, Pair (Bool true, Nil)), Nil) *)
    | Pair (Pair (arg, Pair (_, Nil)), bindings) -> Pair (Pair (arg, (Pair (Pair (Symbol "quote", Pair (Symbol "whatever", Nil)) , Nil))), parseLetRecBindings bindings)
    | _ -> raise X_syntax_error

  and parseLetRecBody bindings body =
    match bindings with
    | Nil -> body
    (* | Pair (Pair (arg, Pair (v, Nil)), Nil) -> Pair (Pair (Symbol "set!", Pair (arg, Pair (v, Nil))), body) *)
    | Pair (Pair (arg, Pair (v, Nil)), bindings) -> Pair (Pair (Symbol "set!", Pair (arg, Pair (v, Nil))), parseLetRecBody bindings body)
    | _ -> raise X_syntax_error

  and parseLambda args bodies =
    if isSimpleLambda args
    then parseLambdaSimple args bodies
    else parseLambdaOpt args bodies

  and isSimpleLambda =
    function
    | Nil -> true
    | Pair (Symbol _, Symbol _) -> false
    | Pair (Symbol _, x) -> isSimpleLambda x
    | Symbol x -> false
    | _ -> raise X_syntax_error

  (* and sequencesImplicitExpr =
     function
     | Nil -> []
     (* | Pair (hd, Pair (tl, Nil)) -> [tag_parse hd; tag_parse tl] (* not necessary??????? *) *)
     | Pair (hd, tail) -> tag_parse hd :: sequencesImplicitExpr tail
     | _-> raise X_syntax_error *)

  and sequencesExpr bodies =
    match bodies with
    | Nil -> Const Void
    | Pair (body, Nil) -> tag_parse body
    (* | _ -> Seq (sequencesImplicitExpr bodies) *)
    | _ -> Seq (List.map tag_parse (pairToList bodies))

  and parseLambdaSimple args bodies =
    match bodies with
    | Nil -> raise X_syntax_error
    | Pair (body, Nil) -> LambdaSimple (parseLambdaParams args pairToList, tag_parse body)
    | _ ->  LambdaSimple (parseLambdaParams args pairToList, sequencesExpr bodies)

  and parseLambdaOpt args bodies =
    match bodies with
    | Nil -> raise X_syntax_error
    | Pair (body, Nil) -> LambdaOpt ((List.rev (List.tl (List.rev (parseLambdaParams args pairToListOpt)))),
                                     (List.hd (List.rev (parseLambdaParams args pairToListOpt))), (tag_parse body))
    | _ -> LambdaOpt ((List.rev (List.tl (List.rev (parseLambdaParams args pairToListOpt)))),
                      (List.hd (List.rev (parseLambdaParams args pairToListOpt))), sequencesExpr bodies)

  and parseLambdaParams params pairToListFunc =
    (* let lst = pairToListFunc params in *)
    List.map (fun param ->
        match param with
        | Symbol str ->
          if List.mem str reserved_word_list
          then raise X_syntax_error
          else str
        | _ -> raise X_syntax_error)
      (duplicateCheck (pairToListFunc params))

  and duplicateCheck lst =
    let rec check =
      function
      | [] -> lst
      | car :: cdr ->
        if List.mem car cdr
        then raise X_syntax_error
        else check cdr
    in
    check lst

  (* and duplicateCheck lst originalList =
     if lst = []
     then originalList
     else
     if List.mem (List.hd lst) (List.tl lst)
     then raise X_syntax_error
     else duplicateCheck (List.tl lst) originalList *)

  and pairToListOpt =
    function
    | Pair (left, Pair (left2, right2)) -> left :: pairToListOpt (Pair (left2, right2))
    | Pair (left, right) -> left :: [right]
    | Symbol x -> [Symbol x]
    | _ -> raise X_syntax_error
  ;;

  let tag_parse_expression sexpr = tag_parse sexpr;;

  let tag_parse_expressions sexprs = List.map tag_parse_expression sexprs;;

end;;

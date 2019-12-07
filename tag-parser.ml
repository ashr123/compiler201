#use "reader.ml";;

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
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                          (expr_eq th1 th2) &&
                                          (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                           (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
    (List.for_all2 String.equal vars1 vars2) &&
    (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
    (String.equal var1 var2) &&
    (List.for_all2 String.equal vars1 vars2) &&
    (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
    (expr_eq e1 e2) &&
    (List.for_all2 expr_eq args1 args2)
  | _ -> false;;


exception X_syntax_error;;

module type TAG_PARSER =  sig
  val tag_parse_expression : sexpr -> expr
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser (*: TAG_PARSER*) = struct

  let reserved_word_list =
    ["and"; "begin"; "cond"; "define"; "else";
     "if"; "lambda"; "let"; "let*"; "letrec"; "or";
     "quasiquote"; "quote"; "set!"; "unquote";
     "unquote-splicing"];;

  (* work on the tag parser starts here *)

  let rec tag_parse sexpr =
    match sexpr with
    | TaggedSexpr (name, Pair (Symbol "quote", Pair (data, Nil))) -> Const (Sexpr (TaggedSexpr (name, data)))
    (* | Pair (Symbol "or", lst) -> Or ( tag_parse ) *)
    | Pair (Symbol "if", Pair (test, Pair (dit, Nil))) -> If (tag_parse test, tag_parse dit, Const(Void))
    | Pair (Symbol "if", Pair (test, Pair (dit, Pair (dif, Nil)))) -> If (tag_parse test, tag_parse dit, tag_parse dif)
    | Pair (Symbol "define", Pair (Pair (Symbol name, args), body)) -> tag_parse (Pair (Symbol "define", Pair (Symbol name, Pair (Symbol "lambda", Pair (args, body)))))
    | Pair (Symbol "define", Pair (Symbol name, Pair (sexpr, Nil))) -> Def (tag_parse (Symbol name), tag_parse sexpr)
    | Pair (Symbol "let", Pair (bindings , body)) -> tag_parse (Pair (Pair (Symbol "lambda", Pair (getArgs bindings,body)), getVals bindings))
    | Pair (Symbol "let*", Pair (bindings, body)) -> tag_parse (parseLetStar bindings body)
    | Pair (Symbol "quote", Pair (x, Nil)) -> Const (Sexpr x)
    | Pair (Symbol "lambda", Pair (args, bodies)) -> parseLambda args bodies
    | Pair (exp1, rest) -> Applic ((tag_parse exp1), List.fold_right (fun x acc -> List.cons x acc) (List.map tag_parse (pairToList rest)) [])
    | Number _| Char _| Bool _| String _| TagRef _| TaggedSexpr _ -> Const (Sexpr sexpr)
    | Symbol s -> if (List.mem s reserved_word_list) then raise X_syntax_error else Var s
    | _ -> Const Void

  and varParser str = List.mem str reserved_word_list

  and pairToList =
    function
    | Nil -> []
    | Pair (left, right) -> left :: pairToList right
    | _ -> raise X_syntax_error

  and getArgs =
    function
    | Nil -> Nil
    (* | Pair (Pair(arg,v), Nil) -> Pair(arg, Nil) *)
    | Pair (Pair (arg, Pair (v, Nil)), bindings) -> Pair (arg, getArgs bindings)
    | _ -> raise X_syntax_error

  and getVals =
    function
    | Nil -> Nil
    (* | Pair (Pair(arg,Pair(v,Nil)), Nil) -> Pair(v, Nil) *)
    | Pair (Pair (_, Pair (v, Nil)), bindings) -> Pair (v, getVals bindings)
    | _ -> raise X_syntax_error

  and parseLetStar bindings body =
    match bindings with
    | Nil -> Pair (Symbol "let", Pair (bindings, body))
    | Pair (Pair (arg, Pair (v, Nil)), Nil) -> Pair (Symbol "let", Pair (Pair (arg, Pair (v, Nil)), body))
    | Pair (Pair (arg, Pair (v, Nil)), bindings) -> Pair (Symbol "let", Pair (Pair (arg, Pair (v, Nil)), Pair (Symbol "let*", Pair (bindings, body))))
    | _ -> raise X_syntax_error

  and parseLambda args bodies =
    if ifSimpleLambda args
    then parseLambdaSimple args bodies
    else parseLambdaOpt args bodies

  and ifSimpleLambda =
    function
    | Nil -> true
    | Pair (Symbol _, Symbol _) -> false
    | Pair (Symbol _, x) -> ifSimpleLambda x
    | Symbol x -> false
    | _ -> raise X_syntax_error

  and sequencesImplicitExpr =
    function
    | Nil -> []
    | Pair (hd, Pair (tl, Nil)) -> [tag_parse hd; tag_parse tl]
    | Pair (hd, tail) -> tag_parse hd :: sequencesImplicitExpr tail
    | _-> raise X_syntax_error

  and sequencesExpr bodies =
    match bodies with
    | Nil -> Const Void
    | Pair (body, Nil) -> tag_parse body
    | _ -> Seq (sequencesImplicitExpr bodies)

  and parseLambdaSimple args bodies =
    match bodies with
    | Pair (body, Nil) -> LambdaSimple (parseLambdaParams args pairToList, tag_parse body)
    | _ -> LambdaSimple (parseLambdaParams args pairToList, sequencesExpr bodies)

  and parseLambdaOpt args bodies =
    match bodies with
    | Pair (body, Nil) -> LambdaOpt ((List.rev (List.tl (List.rev (parseLambdaParams args pairToListOpt)))),
                                     (List.hd (List.rev (parseLambdaParams args pairToListOpt))), (tag_parse body))
    | _ -> LambdaOpt ((List.rev (List.tl (List.rev (parseLambdaParams args pairToListOpt)))),
                      (List.hd (List.rev (parseLambdaParams args pairToListOpt))), sequencesExpr bodies)

  and parseLambdaParams params pairToListFunc =
    let lst = pairToListFunc params in
    List.map (fun param ->
        match param with
        | Symbol str ->
          if not (varParser str)
          then str
          else raise X_syntax_error
        | _ -> raise X_syntax_error)
      (duplicateCheck lst (*lst*))

  and duplicateCheck lst =
    let originalList = lst
    in
    let rec check =
      function
      | [] -> originalList
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

  let tag_parse_expressions sexpr = raise X_not_yet_implemented;;

end;; (* struct Tag_Parser *)

(* #use "tag-parser.ml";; *)
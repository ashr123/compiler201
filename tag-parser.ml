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
| Pair (Symbol "define", Pair( Pair(Symbol name, args), body)) ->
              tag_parse (Pair( Symbol "define", Pair( Symbol name, Pair( Symbol "lambda", Pair (args, body)))))
| Pair (Symbol "define", Pair (Symbol name, Pair (sexpr, Nil))) -> Def (tag_parse (Symbol name), tag_parse sexpr)
| Pair (Symbol "quote", Pair (x, Nil)) -> Const (Sexpr x)
| Pair (exp1, rest) -> Applic ((tag_parse exp1), List.fold_right (fun x acc -> List.cons x acc) (pairstoList rest) [])
| Number _| Char _| Bool _| String _| TagRef _| TaggedSexpr _ -> Const (Sexpr sexpr)
| Symbol s -> if (List.mem s reserved_word_list) then raise X_syntax_error else Var s
| _ -> Const Void

and pairstoList =
  function
  | Pair (sexpr, Nil) -> [tag_parse sexpr]
  | Pair (sexpr, pair) -> [tag_parse sexpr] @ pairstoList pair
  | _ -> raise X_syntax_error;;
;;

let tag_parse_expression sexpr = tag_parse sexpr;;

let tag_parse_expressions sexpr = raise X_not_yet_implemented;;
  
end;; (* struct Tag_Parser *)

(* #use "tag-parser.ml";; *)
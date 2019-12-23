#use "tag-parser.ml";;
open Tag_Parser;;

type var = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of expr' * expr'
  | Def' of expr' * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                              (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
  | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
  | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq var1 var2) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
	 (expr'_eq e1 e2) &&
	   (List.for_all2 expr'_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;
(*
module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;;
*)

module Semantics (*: SEMANTICS*) = struct

let rec get_index e lst index =
  if (not (List.mem e lst)) then -1 else
  (if ((List.nth lst index) = e) then index else get_index e lst index+1);;

let rec isBound s bounds = List.mem s (List.flatten bounds);;
let rec getMajorMinor s bounds =
  let (lst,index) = (List.fold_left
                  (fun (lst, indexoflst) b -> (if (List.mem s b)
                                                then (b, indexoflst)
                                                else (lst, indexoflst)))
                  ([], 0) bounds)
  in
  (index, (get_index s lst 0))
  ;;

let rec annotate_lexical_addresses_lambda params bounds expr =
  match expr with
  | Const c -> Const' c
  | Var s -> let index = (get_index s params 0) in
              (if (index > -1) then Var' (VarParam (s, index))
              else
                (if (isBound s bounds)
                then (let (major, minor) = (getMajorMinor s bounds)
                      in Var' (VarBound (s, major, minor)))
                else Var' (VarFree s)))
  | If (test, dit, dif) -> If' (annotate_lexical_addresses_lambda params bounds test, annotate_lexical_addresses_lambda params bounds dit, annotate_lexical_addresses_lambda params bounds dif)
  | Seq exprlist -> Seq' (List.map (fun e -> annotate_lexical_addresses_lambda params bounds e) exprlist)
  | Set (expr1, expr2) -> Set' (annotate_lexical_addresses_lambda params bounds expr1, annotate_lexical_addresses_lambda params bounds expr2)
  | Def (expr1, expr2) -> Def' (annotate_lexical_addresses_lambda params bounds expr1, annotate_lexical_addresses_lambda params bounds expr2)
  | Or exprlist -> Or' (List.map (fun e -> annotate_lexical_addresses_lambda params bounds e) exprlist)
  | LambdaSimple (newParams, expr) -> annotate_lexical_addresses_lambda newParams (List.cons params bounds) expr
  | LambdaOpt (newParams, optional, expr) -> annotate_lexical_addresses_lambda newParams(*add the optional if needed*) bounds(*add the current params*) expr
  | Applic (expr, exprlist) -> Applic' (annotate_lexical_addresses_lambda params bounds expr, (List.map (fun e -> annotate_lexical_addresses_lambda params bounds e) exprlist))
  | _ -> raise X_syntax_error (*lambda without body*)
  ;; 

let rec recursive_annotate_lexical_addresses expr =
  match expr with
  | Const c -> Const' c
  | Var s -> Var' (VarFree s)
  | If (test, dit, dif) -> If' (recursive_annotate_lexical_addresses test, recursive_annotate_lexical_addresses dit, recursive_annotate_lexical_addresses dif)
  | Seq exprlist -> Seq' (List.map recursive_annotate_lexical_addresses exprlist)
  | Set (expr1, expr2) -> Set' (recursive_annotate_lexical_addresses expr1, recursive_annotate_lexical_addresses expr2)
  | Def (expr1, expr2) -> Def' (recursive_annotate_lexical_addresses expr1, recursive_annotate_lexical_addresses expr2)
  | Or exprlist -> Or' (List.map recursive_annotate_lexical_addresses exprlist)
  | LambdaSimple (params, expr) -> LambdaSimple' (params, (annotate_lexical_addresses_lambda params [] expr))
  | LambdaOpt (params, optional, expr) ->(*not done yet :) *) annotate_lexical_addresses_lambda params [] expr
  | Applic (expr, exprlist) -> Applic' (recursive_annotate_lexical_addresses expr, (List.map recursive_annotate_lexical_addresses exprlist))
  | _ -> raise X_syntax_error
;;

let annotate_lexical_addresses e = recursive_annotate_lexical_addresses e;;

let annotate_tail_calls e = raise X_not_yet_implemented;;

let box_set e = raise X_not_yet_implemented;;

let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;
  
end;; (* struct Semantics *)

(*    "(lambda (x) (lambda (y z) (lambda (v) (f z x)) (+ v z x) v))"    *)
(* test to check : "(lambda (x z) (lambda (v) (+ z x)) v)"   *)
Semantics.annotate_lexical_addresses (Tag_Parser.tag_parse_expression
(Reader.read_sexpr "(lambda (x z) (lambda (v) (x)) v)"));;

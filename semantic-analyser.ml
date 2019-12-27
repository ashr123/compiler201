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
  | Applic' of expr' * expr' list
  | ApplicTP' of expr' * expr' list;;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const' (Sexpr s1), Const' (Sexpr s2) -> sexpr_eq s1 s2
  | Var' (VarFree v1), Var' (VarFree v2) -> String.equal v1 v2
  | Var' (VarParam (v1, mn1)), Var' (VarParam (v2, mn2)) -> String.equal v1 v2 &&
                                                            mn1 = mn2
  | Var' (VarBound (v1, mj1, mn1)), Var' (VarBound (v2, mj2, mn2)) -> String.equal v1 v2 &&
                                                                      mj1 = mj2 &&
                                                                      mn1 = mn2
  | If' (t1, th1, el1), If' (t2, th2, el2) -> expr'_eq t1 t2 &&
                                              expr'_eq th1 th2 &&
                                              expr'_eq el1 el2
  | Seq' l1, Seq' l2
  | Or' l1, Or' l2 -> List.for_all2 expr'_eq l1 l2
  | Set' (var1, val1), Set' (var2, val2)
  | Def' (var1, val1), Def' (var2, val2) -> expr'_eq var1 var2 &&
                                            expr'_eq val1 val2
  | LambdaSimple' (vars1, body1), LambdaSimple' (vars2, body2) -> List.for_all2 String.equal vars1 vars2 &&
                                                                  expr'_eq body1 body2
  | LambdaOpt' (vars1, var1, body1), LambdaOpt' (vars2, var2, body2) -> String.equal var1 var2 &&
                                                                        List.for_all2 String.equal vars1 vars2 &&
                                                                        expr'_eq body1 body2
  | Applic' (e1, args1), Applic' (e2, args2)
  | ApplicTP' (e1, args1), ApplicTP' (e2, args2) -> expr'_eq e1 e2 &&
                                                    List.for_all2 expr'_eq args1 args2
  | _ -> false;;


exception X_syntax_error;;

module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;;

module Semantics(* : SEMANTICS*) = struct

  let rec get_index e lst index =
    (*List.fold_left (fun acc e -> if (acc = -1 && (List.nth lst acc) = e) then acc else (acc+1)) -1 lst*)
    if (not (List.mem e lst)) then -1
    else (if ((List.nth lst index) = e) then index else (get_index e lst (index+1)));;

  let rec isBound s bounds = List.iter (fun s-> (Printf.printf "%s" s))  (List.flatten bounds);(Printf.printf "\n"); List.mem s (List.flatten bounds);;
  let rec getMajorMinor s bounds =
    let (lst,index, _) = (List.fold_left
                            (fun (lst, indexoflst, indexacc) b -> (if ((indexoflst = -1) && (List.mem s b))
                                                                   then (b, indexacc, (indexacc+1))
                                                                   else (lst, indexoflst, (indexacc+1) )))
                            ([], -1, 0) bounds )
    in (index, (get_index s lst 0))
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
    | LambdaSimple (newParams, newExpr) -> LambdaSimple' (newParams, (annotate_lexical_addresses_lambda newParams (List.cons params bounds) newExpr))
    | LambdaOpt (newParams, optional, expr) -> 
      let newParams = (List.cons optional newParams)
      in LambdaOpt' (newParams, optional, (annotate_lexical_addresses_lambda newParams (List.cons newParams bounds) expr))
    | Applic (expr, exprlist) -> Applic' ((annotate_lexical_addresses_lambda params bounds expr), (List.map (fun e -> annotate_lexical_addresses_lambda params bounds e) exprlist))
    | _ -> raise X_syntax_error (* lambda without body *)
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
    | LambdaOpt (params, optional, expr) ->  LambdaOpt' (params, optional, annotate_lexical_addresses_lambda (List.cons optional params) [] expr)
    | Applic (expr, exprlist) -> Applic' (recursive_annotate_lexical_addresses expr, (List.map recursive_annotate_lexical_addresses exprlist))
    | _ -> raise X_syntax_error
  ;;

  let rec parseTP (expr' : expr') inTP =
    let getContent bodies =
      let reverseLst = List.rev bodies
      in
      let lastBody = List.hd reverseLst
      and lstWithoutLastBody = List.rev (List.tl reverseLst)
      in
      (List.map (fun body -> parseTP body false) lstWithoutLastBody) @ [parseTP lastBody inTP]
    in
    match expr' with
    | LambdaSimple' (paramsArr, body) -> LambdaSimple' (paramsArr, parseTP body true)
    | LambdaOpt' (paramsArr, lastParm, body) -> LambdaOpt' (paramsArr, lastParm, parseTP body true)
    | If' (test, dit, dif) -> If' (parseTP test false, parseTP dit inTP, parseTP dif inTP)
    | Seq' bodies -> Seq' (getContent bodies)
    | Or' bodies -> Or' (getContent bodies)
    | Set' (expr1', expr2') -> Set' (parseTP expr1' false, parseTP expr2' false)
    | Def' (expr1', expr2') -> Def' (parseTP expr1' false, parseTP expr2' inTP)
    | Applic' (expr', expr'lst) ->
      let expr' = parseTP expr' false
      and expr'lst = List.map (fun expr' -> parseTP expr' false) expr'lst
      in
      if inTP
      then ApplicTP' (expr', expr'lst)
      else Applic' (expr', expr'lst)
    | _ -> expr' (* for vars' and consts' *)
  ;;

  type readOrWrite = Read | Write;;

  let rec check_rw_first_lambda rw body param = (*checking only the body, without going deep in nesting*)
    match body with
    | Const' _ -> true
    | Var' (VarFree s) -> false (*param can't be free var in body*)
    | Var' (VarBound (s, i, j)) -> false (*we are checking the first lambda where param is just a param*)
    | Var' (VarParam (s, i))  -> (rw = Read && s = param)
    | Box' _ | BoxGet' _ | BoxSet' _ -> true
    | If' (test, dit, dif) -> (ormap (fun expr' -> check_rw_first_lambda rw expr' param) [test; dit; dif])
    | Seq' exprlist -> (ormap (fun expr' -> check_rw_first_lambda rw expr' param) exprlist)
    | Set' (expr1, expr2) -> if (rw = Read) then false
                            else (check_rw_first_lambda Read expr1 param || check_rw_first_lambda rw expr2 param)
    | Def' (expr1, expr2) -> ((check_rw_first_lambda rw expr1 param) || (check_rw_first_lambda rw expr2 param))
    | Or' exprlist -> (ormap (fun expr' -> check_rw_first_lambda rw expr' param) exprlist)
    | LambdaSimple' _ | LambdaOpt' _ -> false
    | Applic' (expr, exprlst) | ApplicTP' (expr, exprlst) ->
      ((check_rw_first_lambda rw expr param) || (ormap (fun s -> check_rw_first_lambda rw s param) exprlst))
  ;;

  let rec check_rw_nested_body rw body param major =
    match body with
    | Const' _ -> false
    | Var' (VarFree s) -> false
    | Var' (VarParam (s, i)) -> false
    | Var' (VarBound (s,i,j)) -> (rw=Read && i=major && s=param)
    | Box' _ | BoxGet' _ | BoxSet' _ -> false
    | If' (test, dit, dif) -> (ormap (fun expr' -> check_rw_nested_body rw expr' param major) [test; dit; dif])
    | Seq' exprlist -> (ormap (fun expr' -> check_rw_nested_body rw expr' param major) exprlist)
    | Set' (expr1, expr2) -> if (rw = Read) then false
                            else ((check_rw_nested_body Read expr1 param major) || (check_rw_nested_body rw expr2 param major))
    | Def' (expr1, expr2) -> ((check_rw_nested_body rw expr1 param major) || (check_rw_nested_body rw expr2 param major))
    | Or' exprlist ->  (ormap (fun expr' -> check_rw_nested_body rw expr' param major) exprlist)
    | LambdaSimple' (params, body) -> check_rw_nested_body rw body param (major+1)
    | LambdaOpt' (params, optional, body) -> check_rw_nested_body rw body param (major+1)
    | Applic' (expr, exprlst) | ApplicTP' (expr, exprlst) ->
      ((check_rw_nested_body rw expr param major) || (ormap (fun s -> check_rw_nested_body rw s param major) exprlst))
  ;;
  
  let rec do_box body param major = 
   match body with
    | Const' _ -> body
    | Var' (VarFree s) -> body
    | Var' (VarParam (s,i)) -> if (s=param) then BoxGet' (VarParam (s,i)) else body
    | Var' (VarBound (s,i,j)) -> if (s=param && i=major) then BoxGet' (VarBound (s,i,j)) else body
    | Box' _ | BoxGet' _ | BoxSet' _ -> body
    | If' (test, dit, dif) -> If' (do_box test param major, do_box dit param major, do_box dif param major)
    | Seq' exprlist -> Seq' (List.map (fun expr' -> do_box expr' param major) exprlist)
    | Set' (Var' var, expr2) ->
      (match var with
      | VarFree s -> Set' (Var' var, do_box expr2 param major)
      | VarParam (s,i) -> if (s=param) then BoxSet' (var, do_box expr2 param major) else Set' (Var' var, do_box expr2 param major)
      | VarBound (s,i,j) -> if (s=param && i=major) then BoxSet' (var, do_box expr2 param major) else Set' (Var' var, do_box expr2 param major)
      )
    | Def' (expr1, expr2) -> Def' (do_box expr1 param major, do_box expr2 param major)
    | Or' exprlist -> Or' (List.map (fun expr' -> do_box expr' param major) exprlist)
    | LambdaSimple' (params, body) -> LambdaSimple' (params, do_box body param (major + 1))
    | LambdaOpt' (params, optional, body) -> LambdaOpt' (params, optional, do_box body param (major + 1))
    | Applic' (expr, exprlst) -> Applic' (do_box expr param major, List.map (fun expr' -> do_box expr' param major) exprlst)
    | ApplicTP' (expr, exprlst) -> ApplicTP' (do_box expr param major, List.map (fun expr' -> do_box expr' param major) exprlst)
    | _ -> raise X_syntax_error
  ;;

  let before_do_box body param major minor = 
    let boxed_body = (do_box body param major)
    and prefix = Set'(Var'(VarParam (param,minor)), Box'(VarParam (param,minor)))
    in
    match boxed_body with
    | Seq' exprlst -> Seq' (List.cons prefix exprlst)
    | _ -> Seq' [prefix ; boxed_body]
    ;;

  let box_set_lambda dynamicBody param minor=
    (*check if body of expr' reads/writes param, if not- Salamat*)
    (*check if expr' is lambda, and it's body reads/writes param (check recursivly), if not- Salamat*)
    (*do box*)
    if (((check_rw_first_lambda Read dynamicBody param) && (check_rw_nested_body Write dynamicBody param (-1))) ||
        (check_rw_first_lambda Write dynamicBody param) && (check_rw_nested_body Read dynamicBody param (-1)))
    then (before_do_box dynamicBody param (-1) minor)
    else dynamicBody
  ;;

  let rec recursive_box_set expr' =
    match expr' with
    | Const' _ | Var' _ | Box' _ | BoxGet' _ -> expr'
    | BoxSet' (var, expr') -> BoxSet' (var, recursive_box_set expr')
    | If' (test, dit, dif) -> If' (recursive_box_set test, recursive_box_set dit, recursive_box_set dif)
    | Seq' exprlist -> Seq' (List.map (fun expr' -> recursive_box_set expr') exprlist)
    | Set' (expr1, expr2) -> Set' (recursive_box_set expr1, recursive_box_set expr2)
    | Def' (expr1, expr2) -> Def' (recursive_box_set expr1, recursive_box_set expr2)
    | Or' exprlist -> Or' (List.map (fun expr' -> recursive_box_set expr') exprlist)
    | LambdaSimple' (params, body) ->
      LambdaSimple' (params, List.fold_left (fun dynamicBody param -> box_set_lambda dynamicBody param (get_index param params 0))
      (*WARNING the recursive call here may be dangerous*) (recursive_box_set body) params)
    | LambdaOpt' (params, optional, body) ->
      LambdaOpt' (params, optional, List.fold_left (fun dynamicBody param -> box_set_lambda dynamicBody param (get_index param params 0))
      (*WARNING the recursive call here may be dangerous*) (recursive_box_set body) (List.cons optional params))
    | Applic' (expr, exprlst) -> Applic' (recursive_box_set expr, List.map (fun expr' -> recursive_box_set expr') exprlst)
    | ApplicTP' (expr, exprlst) -> ApplicTP' (recursive_box_set expr, List.map (fun expr' -> recursive_box_set expr') exprlst)

  let annotate_lexical_addresses e = recursive_annotate_lexical_addresses e;;

  let annotate_tail_calls e = parseTP e false;;

  let box_set e = recursive_box_set e;;

  let run_semantics expr =
    box_set
      (annotate_tail_calls
         (annotate_lexical_addresses expr));;

end;; (* struct Semantics *)

(*tests*)
      (*
      Semantics.annotate_lexical_addresses (Tag_Parser.tag_parse_expression
      (Reader.read_sexpr "(lambda (x) (lambda (y z) (lambda (v) (f z x))))"));; *)
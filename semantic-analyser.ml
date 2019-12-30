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

  let rec isBound s bounds = (*List.iter (fun s-> (Printf.printf "%s" s))  (List.flatten bounds); (Printf.printf "\n"); *)
    List.mem s (List.flatten bounds);;
  let rec getMajorMinor s bounds =
    let (lst,index, _) = (List.fold_left
                            (fun (lst, indexoflst, indexacc) b -> (if ((indexoflst = -1) && (List.mem s b))
                                                                   then (b, indexacc, (indexacc+1))
                                                                   else (lst, indexoflst, (indexacc+1) )))
                            ([], -1, 0) bounds )
    in (index, (get_index s lst 0))
  ;;

  let rec print_list = function
      [] -> ()
    | e::l -> Printf.printf "%s " e ; print_list l; Printf.printf "\n"
  ;;

  let rec print_list_int =
    function
    | [] -> print_string "]\n"
    | e :: [] -> print_int e; print_list_int []
    | e :: l -> print_int e; print_string "; "; print_list_int l

  and print_int_int_list =
    function
    | [] -> print_string "]\n"
    | (e1, e2) :: [] -> print_char '('; print_int e1; print_string ", "; print_int e2; print_char ')'; print_int_int_list []
    | (e1, e2) :: li -> print_char '('; print_int e1; print_string ", "; print_int e2; print_char ')'; print_string "; "; print_int_int_list li
  ;;

  let rec annotate_lexical_addresses_lambda params bounds expr =
    match expr with
    | Const c -> Const' c
    | Var s -> let index = (get_index s params 0) in
      (if (index > -1) then Var' (VarParam (s, index))
       else
         (*(Printf.printf "%s %B\n" s (isBound s bounds);
           List.map print_list bounds; *)
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
      let allParams = (newParams @ [optional])
      in LambdaOpt' (newParams, optional, (annotate_lexical_addresses_lambda allParams (List.cons params bounds) expr))
    | Applic (expr, exprlst) -> Applic' ((annotate_lexical_addresses_lambda params bounds expr), (List.map (fun e -> annotate_lexical_addresses_lambda params bounds e) exprlst))
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
    | LambdaOpt (params, optional, expr) ->  LambdaOpt' (params, optional, annotate_lexical_addresses_lambda (params@[optional]) [] expr)
    | Applic (expr, exprlist) -> Applic' (recursive_annotate_lexical_addresses expr, (List.map recursive_annotate_lexical_addresses exprlist))
  ;;

  let rec parseTP (expr' : expr') inTP =
    let map expr'lst = List.map (fun expr' -> parseTP expr' false) expr'lst
    in
    let getContent bodies =
      let reverseLst = List.rev bodies
      in
      let lastBody = List.hd reverseLst
      and lstWithoutLastBody = List.rev (List.tl reverseLst)
      in
      map lstWithoutLastBody @ [parseTP lastBody inTP]
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
      and expr'lst = map expr'lst
      in
      if inTP
      then ApplicTP' (expr', expr'lst)
      else Applic' (expr', expr'lst)
    | _ -> expr' (* for vars' and consts' *)
  ;;


  let rec isNeedToBeBoxed (param : string) (body : expr') : bool =
    let cartesian l l' = List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l)
    and newBody = removeShadow body param []
    in
    let readList = checkRead (counterGen ()) param newBody
    and writeList = checkWrite (counterGen ()) param newBody
    in
    (* print_string "read list: ["; print_list_int readList;
       print_string "write list: ["; print_list_int writeList; *)
    let joinedList = cartesian readList writeList
    in
    (* print_string "joined list: ["; print_int_int_list joinedList; *)
    let res = List.fold_right (fun (readNum, writeNum) res -> res || (readNum <> writeNum)) joinedList false in
    (* print_string param; print_boolean res; *)
    res

  and checkRead (count : bool -> int) (param : string) (body : expr') : int list =
    match body with
    | LambdaSimple' (_, body)|LambdaOpt' (_, _, body) ->
      let c = count true in
      if checkRead count param body <> []
      then [c]
      else []
    | Applic' (expr', expr'List)|ApplicTP' (expr', expr'List) ->
      checkRead count param expr' @ List.fold_right (fun expr' lst -> lst @ checkRead count param expr') expr'List []
    | Seq' expr'List -> List.fold_right (fun expr' lst -> lst @ checkRead count param expr') expr'List []
    | If' (test, dit, dif) -> checkRead count param test @ checkRead count param dit @ checkRead count param dif
    | Def' (expr1', expr2') -> checkRead count param expr1' @ checkRead count param expr2'
    | Or' expr'List -> List.fold_right (fun expr' lst -> lst @ checkRead count param expr') expr'List []
    | Set' (expr1', expr2') -> checkRead count param expr2'
    | Var' str ->
      (match str with
       | VarParam (x, _)|VarBound (x, _, _) ->
         if x = param
         then [-2]
         else []
       | VarFree _ -> [])
    | Const' _ -> []
    | _ -> raise X_syntax_error

  and checkWrite count param body =
    match body with
    | LambdaSimple' (_, body)|LambdaOpt' (_, _, body) ->
      let c = count true in
      if checkWrite count param body <> []
      then [c]
      else []
    | Applic' (expr', expr'List)|ApplicTP' (expr', expr'List) ->
      checkWrite count param expr' @ List.fold_right (fun expr' lst -> lst @ checkWrite count param expr') expr'List []
    | Seq' expr'List -> List.fold_right (fun expr' lst -> lst @ checkWrite count param expr') expr'List []
    | If' (test, dit, dif) -> checkWrite count param test @ checkWrite count param dit @ checkWrite count param dif
    | Def' (expr1', expr2') -> checkWrite count param expr1' @ checkWrite count param expr2'
    | Or' expr'List -> List.fold_right (fun expr' lst -> lst @ checkWrite count param expr') expr'List []
    | Set' (expr1', expr2') ->
      (match expr1' with
       | Var' (VarParam (x, _))|Var' (VarBound (x, _, _)) ->
         if x = param
         then [-2]
         else []
       | Var' (VarFree _) -> []
       | _ -> raise X_syntax_error) @ checkWrite count param expr2'
    | Var' _ -> []
    | Const' _ -> []
    | _ -> raise X_syntax_error

  and removeShadow body param paramsAcc =
    match body with
    | LambdaSimple' (paramsArr, body) -> LambdaSimple' (paramsArr, removeShadow body param (paramsAcc @ paramsArr))
    | LambdaOpt' (paramsArr, lastParm, body) -> LambdaOpt' (paramsArr, lastParm, removeShadow body param (paramsAcc @ (paramsArr @ [lastParm])))
    | If' (test, dit, dif) -> If' (removeShadow test param paramsAcc, removeShadow dit param paramsAcc, removeShadow dif param paramsAcc)
    | Seq' bodies ->
      Seq' (List.map (fun body -> removeShadow body param paramsAcc) bodies)
    | Or' bodies ->
      Or' (List.map (fun body -> removeShadow body param paramsAcc) bodies)
    | Set' (expr1', expr2') ->
      Set' ((match expr1' with
          | Var' (VarParam (name, minorIndex)) ->
            if List.mem param paramsAcc && name = param
            then Var' (VarParam (name ^ "$", minorIndex))
            else Var' (VarParam (name, minorIndex))
          | Var' (VarBound (name, majorIndex, minorIndex)) ->
            if List.mem param paramsAcc && name = param
            then Var' (VarBound (name ^ "$", majorIndex, minorIndex))
            else Var' (VarBound (name, majorIndex, minorIndex))
          | Var' (VarFree x)-> Var' (VarFree x)
          | _ -> raise X_syntax_error), removeShadow expr2' param paramsAcc)
    | Def' (expr1', expr2') -> Def' (removeShadow expr1' param paramsAcc, removeShadow expr2' param paramsAcc)
    | Applic' (expr', expr'List) ->
      Applic' (removeShadow expr' param paramsAcc, List.map (fun expr' -> removeShadow expr' param paramsAcc) expr'List)
    | ApplicTP' (expr', expr'List) ->
      ApplicTP' (removeShadow expr' param paramsAcc, List.map (fun expr' -> removeShadow expr' param paramsAcc) expr'List)
    | Var' (VarParam (name, minorIndex)) ->
      if List.mem param paramsAcc && name = param
      then Var' (VarParam (name ^ "$", minorIndex))
      else Var' (VarParam (name, minorIndex))
    | Var' (VarBound (name, majorIndex, minorIndex)) ->
      if List.mem param paramsAcc && name = param
      then Var' (VarBound (name ^ "$", majorIndex, minorIndex))
      else Var' (VarBound (name, majorIndex, minorIndex))
    | _x -> _x (* for consts' *)

  and updateParamList paramList = List.map (fun (varName, majorIndex) -> (varName, majorIndex + 1)) paramList

  and addBoxParam paramList paramsArr body = List.fold_right (fun p lst ->
      (* print_string "BOOL: "; print_boolean (isNeedToBeBoxed param body); print_char '\n'; *)
      if isNeedToBeBoxed p body
      then  lst @ [(p, -1)]
      else lst) paramsArr paramList

  and  print_pair_list = function
    | [] -> print_string "]\n"
    | (name, majorIndex) :: [] -> print_string "(\""; print_string name; print_string "\", "; print_int majorIndex; print_string ")"; print_pair_list []
    | (name, majorIndex) :: li -> print_string "(\""; print_string name; print_string "\", "; print_int majorIndex; print_string "); "; print_pair_list li

  and parseBoxing expr' paramsList =
    let makeSeq' boxParam paramsArr body =
      Seq' ((List.fold_right (fun (name, majorIndex) acc ->
          if majorIndex = -1
          then let minor = find name paramsArr 0 in
            (* print_string "paramsList :["; print_pair_list paramsArr; *)
            acc @ [Set' (Var' (VarParam (name, minor)), Box' (VarParam (name, minor)))]
          else acc) boxParam []) @ [body]) in
    match expr' with
    | Seq' bodies -> Seq' (List.map (fun expr' -> parseBoxing expr' paramsList) bodies)
    | Or' bodies -> Or' (List.map (fun expr' -> parseBoxing expr' paramsList) bodies)
    | If' (test, dit, dif) ->
      If' (parseBoxing test paramsList, parseBoxing dit paramsList, parseBoxing dif paramsList)
    | Def' (expr1', expr2') ->
      Def' (parseBoxing expr1' paramsList, parseBoxing expr2' paramsList)
    | Applic' (expr', expr'List) ->
      Applic' (parseBoxing expr' paramsList, List.map (fun expr' -> parseBoxing expr' paramsList) expr'List)
    | ApplicTP' (expr', expr'List) ->
      ApplicTP' (parseBoxing expr' paramsList, List.map (fun expr' -> parseBoxing expr' paramsList) expr'List)
    | LambdaSimple' (paramsArr, body) ->
      let boxParam = addBoxParam (updateParamList paramsList) paramsArr body in
      if List.exists (fun (_, index) -> index = -1) boxParam
      then LambdaSimple' (paramsArr, makeSeq' boxParam paramsArr (parseBoxing body boxParam))
      else LambdaSimple' (paramsArr, parseBoxing body boxParam)
    | LambdaOpt' (paramsArr, lastParam, body) ->
      let joinedList = (paramsArr @ [lastParam]) in
      let boxParam = addBoxParam (updateParamList paramsList) joinedList body in
      if List.exists (fun (_, index) -> index = -1) boxParam
      then LambdaOpt' (paramsArr, lastParam, makeSeq' boxParam joinedList (parseBoxing body boxParam))
      else LambdaOpt' (paramsArr, lastParam, parseBoxing body boxParam)
    | Set' (Var' x, expr2') ->
      (match x with
       | VarParam (name, minorIndex) ->
         if List.mem (name, -1) paramsList
         then BoxSet' (VarParam (name, minorIndex), parseBoxing expr2' paramsList)
         else Set' (Var' x, parseBoxing expr2' paramsList)
       | VarBound (name, majorIndex, minorIndex) ->
         if List.mem (name, majorIndex) paramsList
         then BoxSet' (VarBound (name, majorIndex, minorIndex), parseBoxing expr2' paramsList)
         else Set' (Var' x, parseBoxing expr2' paramsList)
       | _ -> Set' (Var' x, parseBoxing expr2' paramsList))
    | Var' (VarParam (name, minorIndex)) ->
      if List.mem (name, -1) paramsList
      then BoxGet' (VarParam (name, minorIndex))
      else Var' (VarParam (name, minorIndex))
    | Var' (VarBound (name, majorIndex, minorIndex)) ->
      if List.mem (name, majorIndex ) paramsList
      then BoxGet' (VarBound (name, majorIndex, minorIndex))
      else Var' (VarBound (name, majorIndex, minorIndex))
    | _x -> _x (* for consts' *)

  and find x lst index =
    match lst with
    | [] -> -1
    | h :: t ->
      if x = h
      then index
      else find x t (index + 1)

  and counterGen () =
    let count = ref (-1)
    in
    fun toIncr ->
      if toIncr
      then
        (incr count;
         !count)
      else
        !count
  ;;

(*
  type readOrWrite = Read | Write;;

  let rec check_rw_first_lambda rw body param = (*checking only the body, without going deep in nesting*)
    match body with
    | Const' _ -> false
    | Var' (VarFree s) -> false (*param can't be free var in body*)
    | Var' (VarBound (s, i, j)) -> false (*we are checking the first lambda where param is just a param*)
    | Var' (VarParam (s, i))  -> (rw = Read && s = param)
    | Box' _ | BoxGet' _ -> false
    | BoxSet' (var,expr) -> check_rw_first_lambda rw expr param
    | If' (test, dit, dif) -> (ormap (fun expr' -> check_rw_first_lambda rw expr' param) [test; dit; dif])
    | Seq' exprlist -> (ormap (fun expr' -> check_rw_first_lambda rw expr' param) exprlist)
    | Set' (expr1, expr2) ->
      if (rw=Read)
      then (check_rw_first_lambda Read expr2 param)
      else (check_rw_first_lambda Read expr1 param) || (check_rw_first_lambda Write expr2 param)
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
    | Box' _ | BoxGet' _ -> false
    | BoxSet' (var, expr) -> check_rw_nested_body rw expr param major
    | If' (test, dit, dif) -> (ormap (fun expr' -> check_rw_nested_body rw expr' param major) [test; dit; dif])
    | Seq' exprlist -> (ormap (fun expr' -> check_rw_nested_body rw expr' param major) exprlist)
    | Set' (expr1, expr2) ->
      if (rw=Read)
      then (check_rw_nested_body Read expr2 param major)
      else (check_rw_nested_body Read expr1 param major) || (check_rw_nested_body Write expr2 param major)
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
    | Box' _ | BoxGet' _ -> body
    | BoxSet' (var, expr) -> BoxSet' (var, do_box expr param major)
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

  let wrap_body body params =
    let prefix = List.map
        (fun param -> let minor = (get_index param params 0) in Set'(Var'(VarParam (param,minor)), Box'(VarParam (param,minor))))
        params
    in
    match params with
    | [] -> body
    | _ -> Seq' (prefix @ [body])
  ;;

  let what_params_to_box dynamicBody params=
    (*check if body of expr' reads/writes param, if not- Salamat*)
    (*check if expr' is lambda, and it's body reads/writes param (check recursivly), if not- Salamat*)
    (*do box*)
    let condition param = (((check_rw_first_lambda Read dynamicBody param) && (check_rw_nested_body Write dynamicBody param (-1))) ||
                           (check_rw_first_lambda Write dynamicBody param) && (check_rw_nested_body Read dynamicBody param (-1)))
    (*read/write both not in the first lambda but בלי אב קדמון משותף שבו הפרמטר bound*)
    (*and condition2 param =*) (*get read, write majors and indexes of relevant lambdas
                                                                             if somewhere there is אב קדמון משותף שבו המשתנה bound- stop searching*)
    in (List.fold_left (fun acc param -> (if (condition param) then (acc@[param]) else acc)) [] params)
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
      let params_to_box = (what_params_to_box body params)
      in (let body = (List.fold_left (fun newBody param -> do_box newBody param (-1)) (recursive_box_set body) params_to_box)
          in LambdaSimple' (params, wrap_body body params_to_box))
    | LambdaOpt' (params, optional, body) ->
      let params_to_box = (what_params_to_box body (params@[optional]))
      in (let body =  List.fold_left (fun newBody param -> do_box newBody param (-1)) (recursive_box_set body) params_to_box
          in LambdaOpt' (params, optional, wrap_body body params_to_box))
    | Applic' (expr, exprlst) -> Applic' (recursive_box_set expr, List.map (fun expr' -> recursive_box_set expr') exprlst)
    | ApplicTP' (expr, exprlst) -> ApplicTP' (recursive_box_set expr, List.map (fun expr' -> recursive_box_set expr') exprlst)
*)

  let annotate_lexical_addresses e = recursive_annotate_lexical_addresses e;;

  let annotate_tail_calls e = parseTP e false;;

  let box_set e = parseBoxing e [] (*recursive_box_set e*);;

  let run_semantics expr =
    box_set
      (annotate_tail_calls
         (annotate_lexical_addresses expr));;

end;; (* struct Semantics *)

(*tests*)
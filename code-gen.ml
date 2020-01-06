#use "semantic-analyser.ml";;
(* הגדלים של כל מיני סקים אובג'קטזTODO*)

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
       For example: [(Sexpr(Nil), (1, "SOB_NIL"))]
  *)
  val make_consts_tbl : expr' list -> (constant * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
       For example: [("boolean?", 0)]
  *)
  val make_fvars_tbl : expr' list -> (string * int) list

  (* This signature represents the idea of outputing assembly code as a string
     for a single AST', given the full constants and fvars tables.
  *)
  val generate : (constant * (int * string)) list -> (string * int) list -> expr' -> string
end;;

module Code_Gen : CODE_GEN = struct

  (*return -1 if const is not in the table, otherwise returns the offset in the table*)
  let get_offset_of_const table const =
    match const with
    | Void -> 0
    | Sexpr sexpr -> List.fold_left
                       (fun result (const1,(offset1,s1)) ->
                          match const1 with
                          | Void -> result
                          | Sexpr sexpr1 -> if (result>(-1)) then result else
                            if (sexpr_eq sexpr sexpr1)
                            then offset1
                            else result) (-1) table
  ;;
  (*gets constants table and constant, adds if not included*)
  let add_to_table : (constant*(int*string)) list -> (constant*(int*string)) -> int -> ((constant*(int*string)) list * int) =
    fun table (const,(offset,s)) offsetToAdd ->
      let offset_in_table = get_offset_of_const table const
      in
      if (offset_in_table=(-1)) then (table@[(const,(offset,s))], offset+offsetToAdd) else (table, offset)
  ;;

  (*gets one constant and returns pair of: table with new constant if not exists in the table, and the current offset after adding*)
  let rec constant_of_sexpr const table offset =
    match const with
    | Void -> (table, offset)
    | Sexpr (sexpr) ->
      match sexpr with
      | Nil | Bool _ -> (table, offset) (*these constants are already defined in the table*)
      | Number (Int i) -> let constant = (const,(offset,"MAKE_LITERAL_INT(" ^ (string_of_int i) ^ ")")) in (add_to_table table constant 9)
      | Number (Float f) -> let constant = (const,(offset,"MAKE_LITERAL_FLOAT(" ^ (string_of_float f) ^ ")")) in (add_to_table table constant 9)
      | Char c -> let constant = (const,(offset,"MAKE_LITERAL_CHAR('" ^ (String.make 1 c) ^ "')")) in (add_to_table table constant 2)
      | String s -> let constant = (const,(offset,"MAKE_LITERAL_STRING(\"" ^ s ^ "\")")) in (add_to_table table constant (9+String.length s))
      (* maybe needed '\0' at end of string*)
      | Symbol s ->
        let offsetOfString = get_offset_of_const table (Sexpr (String s))
        in let (table,offsetOfString) = if (offsetOfString=(-1)) then constant_of_sexpr (Sexpr (String s)) table offset else (table,offsetOfString)
        in let constant = (const,(offset,"MAKE_LITERAL_SYMBOL(const_tbl+" ^ (string_of_int offsetOfString) ^ ")"))
        in (add_to_table table constant 9)
      | Pair (sexpr1,sexpr2) -> let (table, offset) = constant_of_sexpr (Sexpr (sexpr1)) table offset
        in let (table, offset) = constant_of_sexpr (Sexpr sexpr2) table offset
        in let constPair = (const, (offset, "MAKE_LITERAL_PAIR(const_tbl+" ^ (string_of_int (get_offset_of_const table (Sexpr sexpr1)))
                                            ^ ", const_tbl+" ^ (string_of_int (get_offset_of_const table (Sexpr sexpr2)))))
        in (add_to_table table constPair 17)
      | TaggedSexpr (s,sexpr1) -> let (table, offset) = (let constant = (const,(offset,"MAKE_LITERAL_STRING(\"" ^ s ^ "\")")) in (add_to_table table constant ((9+String.length s))))
        in (constant_of_sexpr (Sexpr sexpr1) table offset)
      | TagRef s ->  let constant = (const,(offset,"MAKE_LITERAL_STRING(\"" ^ s ^ "\")")) in (add_to_table table constant ((9+String.length s)))
  ;;

  (*gets one expr' and returns list of (constant*(int*string)) *)
  let rec add_constants_to_table ast table offset =
    match ast with
    (*this is the first constants defined in the table*)
    | Const' c -> constant_of_sexpr c table offset
    | Var' _ | Box' _ | BoxGet' _ -> (table, offset)
    | BoxSet' (var, expr') -> add_constants_to_table expr' table offset
    | If' (test, dit, dif) -> List.fold_left (fun (table,offset) ast -> add_constants_to_table ast table offset) (table,offset) [test; dit; dif]
    | Seq' exprlist -> List.fold_left (fun (table,offset) ast -> add_constants_to_table ast table offset) (table,offset) exprlist
    | Set' (expr1, expr2) -> List.fold_left (fun (table,offset) ast -> add_constants_to_table ast table offset) (table,offset) [expr1; expr2]
    | Def' (expr1, expr2) -> List.fold_left (fun (table,offset) ast -> add_constants_to_table ast table offset) (table,offset) [expr1; expr2]
    | Or' exprlist -> List.fold_left (fun (table,offset) ast -> add_constants_to_table ast table offset) (table,offset) exprlist
    | LambdaSimple' (params, body) -> add_constants_to_table body table offset
    | LambdaOpt' (params, optional, body) -> add_constants_to_table body table offset
    | Applic' (expr, exprlst) | ApplicTP' (expr, exprlst) ->
      List.fold_left (fun (table,offset) ast -> add_constants_to_table ast table offset) (table,offset) (exprlst@[expr])
  ;;

  let make_consts_tbl asts =
    let (table, offset) = List.fold_left (fun (table,offset) ast -> (add_constants_to_table ast table offset))
        ([(Void, (0, "MAKE_VOID"));
          (Sexpr(Nil), (1, "MAKE_NIL"));
          (Sexpr(Bool false), (2, "MAKE_BOOL(0)"));
          (Sexpr(Bool true), (4, "MAKE_BOOL(1)"))] , 6)
        asts
    in table
  ;;

  (* returns the new (table,offset)*)
  let rec add_to_freevars_table table offset ast =
    let doIfNotExists var = match var with
      | VarParam _ | VarBound _ -> (table, offset)
      | VarFree s -> if List.exists (fun (str, _) -> str = s) table
        then (table, offset)
        else (table @ [(s, offset)], offset + 8)
    in
    match ast with
    | Const' _ -> (table, offset)
    | Var' var ->  doIfNotExists var
    | Box' var | BoxGet' var -> doIfNotExists var
    | BoxSet' (var, expr') -> let (table, offset) = doIfNotExists var in add_to_freevars_table table offset expr'
    | If' (test, dit, dif) -> List.fold_left (fun (table, offset) expr' -> add_to_freevars_table table offset expr') (table, offset) [test; dit; dif]
    | Seq' exprlist -> List.fold_left (fun (table, offset) expr' -> add_to_freevars_table table offset expr') (table, offset) exprlist
    | Set' (expr1, expr2) -> List.fold_left (fun (table, offset) expr' -> add_to_freevars_table table offset expr') (table, offset) [expr1; expr2]
    | Def' (expr1, expr2) -> List.fold_left (fun (table, offset) expr' -> add_to_freevars_table table offset expr') (table, offset) [expr1; expr2]
    | Or' exprlist -> List.fold_left (fun (table, offset) expr' -> add_to_freevars_table table offset expr') (table, offset) exprlist
    | LambdaSimple' (strLst, body) -> add_to_freevars_table table offset body
    | LambdaOpt' (params, optional, body) -> add_to_freevars_table table offset body
    | Applic' (expr1, exprlist) | ApplicTP' (expr1, exprlist) -> List.fold_left (fun (table, offset) expr' -> add_to_freevars_table table offset expr') (table, offset) (exprlist @ [expr1])

  let make_fvars_tbl asts =
    let procedures = ["append"; "apply"; "<"; "="; ">"; "+"; "/"; "*"; "-"; "boolean?"; "car"; "cdr";
                      "char->integer"; "char?"; "cons"; "eq?"; "equal?"; "fold-left"; "fold-right"; "integer?"; "integer->char"; "length"; "list"; "list?";
                      "make-string"; "map"; "not"; "null?"; "number?"; "pair?"; "procedure?"; "float?"; "set-car!"; "set-cdr!";
                      "string->list"; "string-length"; "string-ref"; "string-set!"; "string?"; "symbol?"; "symbol->string";
                      "zero?"]
    in
    let (table, offset) = List.fold_left (fun (table, offset) proc -> (table @ [(proc, offset)], offset + 8)) ([], 0) procedures
    in
    let (table, offset) = List.fold_left (fun (table, offset) ast -> (add_to_freevars_table table offset ast)) (table, offset) asts
    in table
  ;;

  let getSizeOfConst =
    function
    | Void
    | Sexpr Nil -> 1
    | Sexpr (Bool _)
    | Sexpr (Char _) -> 1 + 1
    | Sexpr (Number _)
    | Sexpr (Symbol _)
    | Sexpr (TagRef _) -> 1 + 8 (*raise X_not_yet_implemented ???*)
    | Sexpr (Pair _)
    | Sexpr (TaggedSexpr _) -> 1 + 8 + 8 (*raise X_not_yet_implemented ???*)
    | Sexpr (String str) -> 1 + 8 + String.length str
  ;;

  (* https://stackoverflow.com/a/19338726/7997683 *)
  let counterGenerator label =
    let count = ref (-1)  (*in the first time, inc is done and then returned*)
    in
    fun isToIncrease ->
      if isToIncrease
      then incr count;
      label ^ string_of_int !count (* this is outside the if expression *)
  ;;

  let label_Lelse_counter = counterGenerator "Lelse"
  and label_Lexit_counter = counterGenerator "Lexit";;
  ;;

  (* creates assembly code for single expr', these strings will concat
     the prolog will contains the section .data init of const_tbl and freevar_tbl *)
  let generate consts fvars e =
    let rec generateRec consts fvars e =
      match e with
      | Const' constant -> "mov rax, const_tbl + " ^ string_of_int (get_offset_of_const consts constant) ^ "\n"
      | Seq' exprlist -> List.fold_left (fun acc expr' -> acc ^ generateRec consts fvars expr') "" exprlist
      (* very very important !!!!!
         the labels generator will evaluate in undefined order, so make sure by hand that all calls to counter are in the right order *)
      | If' (test, dit, dif) ->
        let elseLabelWithInc = label_Lelse_counter true
        and exitLabelWithInc = label_Lexit_counter true
        in
        generateRec consts fvars test ^
        "cmp rax, SOB_FALSE_ADDRESS\n" ^
        "je " ^ elseLabelWithInc ^ "\n" ^
        generateRec consts fvars dit ^
        "jmp " ^ exitLabelWithInc ^ "\n" ^
        label_Lelse_counter false ^ ":\n" ^
        generateRec consts fvars dif ^
        label_Lexit_counter false ^ ":\n"
      | _ -> raise X_not_yet_implemented
    in
    generateRec consts fvars e ;;
  (* | Var' var ->
     | Box' var ->
     | BoxGet' var ->
     | BoxSet' (var, expr') ->
     | If' (test,dit * dif) ->
     | Seq' exprlist ->
     | Set' (expr1, expr2) ->
     | Def' (expr1, expr2) ->
     | Or' exprlist ->
     | LambdaSimple' (params, body) ->
     | LambdaOpt' of (params, optional, body)->
     | Applic' (expr', exprlist) ->
     | ApplicTP' (expr', exprlist) ->
     ;; *)
end;;

(*tests*)
(*
let expr' = (LambdaSimple' (["a"; "b"; "c"; "d"],
Seq'
 [Set' (Var' (VarParam ("d", 3)), Box' (VarParam ("d", 3)));
  Seq'
   [LambdaSimple' (["a"; "b"; "c"],
     LambdaSimple' ([],
      LambdaSimple' ([],
       Seq'
        [BoxSet' (VarBound ("d", 2, 3),
          Applic' (Var' (VarFree "+"),
           [BoxGet' (VarFree ("d")); Const' (Sexpr (Number (Int 1)))]));
         Set' (Var' (VarFree ("c")),
          Applic' (Const' (Sexpr (Number (Int 2))), []))])));
    LambdaSimple' ([], Var' (VarBound ("c", 0, 2)));
    LambdaSimple' ([],
     BoxSet' (VarBound ("d", 0, 3), Const' (Sexpr (Number (Int 1)))))]]));;
Code_Gen.make_fvars_tbl [expr'];;
*)
(* Code_Gen.label_Lelse_counter true;;
   Code_Gen.label_Lelse_counter true;;
   Code_Gen.label_Lelse_counter true;;
   Code_Gen.label_Lelse_counter false;;
   Code_Gen.label_Lexit_counter true;;
   Code_Gen.label_Lexit_counter true;;
   Code_Gen.label_Lexit_counter true;; *)
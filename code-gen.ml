#use "semantic-analyser.ml";;

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

  (*return -1 if sexpr is not in the table, otherwise returns the offset in the table*)
  let get_offset_of_const table sexpr = List.fold_left
    (fun result (sexpr1,(offset1,s1)) -> if (result>(-1)) then result else
                                      if (sexpr_eq sexpr sexpr1)
                                      then offset1
                                      else result) (-1) table
  ;;
  (*gets constants table and constant, adds if not included*)
  let add_to_table table (sexpr,(offset,s)) offsetToAdd =
    let offset_in_table = get_offset_of_const table sexpr
    in
    if (offset_in_table=(-1)) then (table@[(sexpr,(offset,s))], offset+offsetToAdd) else (table, offset)
  ;;

  (*gets one sexpr and returns pair of: table with new constant if not exists in the table, and the current offset after adding*)
  let rec constant_of_sexpr sexpr table offset =
    match sexpr with
    | Nil | Bool _ -> (table, offset) (*these constants are already defined in the table*)
    | Number (Int i) -> let constant = (sexpr,(offset,"MAKE_LITERAL_INT(" ^ (string_of_int i) ^ ")")) in (add_to_table table constant 9)
    | Number (Float f) -> let constant = (sexpr,(offset,"MAKE_LITERAL_FLOAT(" ^ (string_of_float f) ^ ")")) in (add_to_table table constant 9)
    | Char c -> let constant = (sexpr,(offset,"MAKE_LITERAL_CHAR('" ^ (String.make 1 c) ^ "')")) in (add_to_table table constant 2)
    | String s -> let constant = (sexpr,(offset,"MAKE_LITERAL_STRING(" ^ s ^ ")")) in (add_to_table table constant 10)
    | Symbol s -> let constant = (sexpr,(offset,"MAKE_LITERAL_SYMBOL(" ^ s ^ ")")) in (add_to_table table constant 9)
    | Pair (sexpr1,sexpr2) -> let (table, offset) = constant_of_sexpr sexpr1 table offset
                              in let (table, offset) = constant_of_sexpr sexpr2 table offset
                              in let constPair = (sexpr, (offset, "MAKE_LITERAL_PAIR(consts+" ^ (string_of_int (get_offset_of_const table sexpr1))
                                                                  ^ ", consts+" ^ (string_of_int (get_offset_of_const table sexpr2))))
                              in (add_to_table table constPair 17)
    | TaggedSexpr (s,sexpr1) -> let (table, offset) = (let constant = (sexpr,(offset,"MAKE_LITERAL_STRING(" ^ s ^ ")")) in (add_to_table table constant 10))
                              in (constant_of_sexpr sexpr1 table offset)
    | TagRef s ->  let constant = (sexpr,(offset,"MAKE_LITERAL_STRING(" ^ s ^ ")")) in (add_to_table table constant 10)
  ;;

  (*gets one expr' and returns list of (constant*(int*string)) *)
  let rec add_constants_to_table ast table offset =
      match ast with
      (*this is the first constants defined in the table*)
      | Const' (Void) -> (table, offset)
      | Const' (Sexpr s) -> constant_of_sexpr s table offset
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
                  ( [ (Void, (0, "MAKE_VOID"));
                    (Sexpr(Nil), (1, "MAKE_NIL"));
                    (Sexpr(Bool false), (2, "MAKE_BOOL(0)"));
                    (Sexpr(Bool true), (4, "MAKE_BOOL(1)")); ] , 6)
                  asts
    in table
  ;;

  let make_fvars_tbl asts = raise X_not_yet_implemented;;
  let generate consts fvars e = raise X_not_yet_implemented;;
end;;


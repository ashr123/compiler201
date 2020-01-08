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
    let make_string s = ("MAKE_LITERAL_STRING(" ^ (string_of_int (String.length s)) ^ "," ^ "\"" ^ s ^ "\")")
    in
    match const with
    | Void -> (table, offset)
    | Sexpr (sexpr) ->
      match sexpr with
      | Nil | Bool _ -> (table, offset) (*these constants are already defined in the table*)
      | Number (Int i) -> let constant = (const,(offset,"MAKE_LITERAL_INT(" ^ (string_of_int i) ^ ")")) in (add_to_table table constant 9)
      | Number (Float f) -> let constant = (const,(offset,"MAKE_LITERAL_FLOAT(" ^ (string_of_float f) ^ ")")) in (add_to_table table constant 9)
      | Char c -> let constant = (const,(offset,"MAKE_LITERAL_CHAR('" ^ (String.make 1 c) ^ "')")) in (add_to_table table constant 2)
      | String s -> let constant = (const,(offset, make_string s)) in (add_to_table table constant (9+String.length s))
      (* maybe needed '\0' at end of string*)
      | Symbol s ->
        let offsetOfString = get_offset_of_const table (Sexpr (String s))
        in let (table,offsetOfString) = if (offsetOfString=(-1)) then constant_of_sexpr (Sexpr (String s)) table offset else (table,offsetOfString)
        in let constant = (const,(offset,"MAKE_LITERAL_SYMBOL(const_tbl+" ^ (string_of_int offsetOfString) ^ ")"))
        in (add_to_table table constant 9)
      | Pair (sexpr1,sexpr2) -> let (table, offset) = constant_of_sexpr (Sexpr (sexpr1)) table offset
        in let (table, offset) = constant_of_sexpr (Sexpr sexpr2) table offset
        in let constPair = (const, (offset, "MAKE_LITERAL_PAIR(const_tbl+" ^ (string_of_int (get_offset_of_const table (Sexpr sexpr1))) ^ ", const_tbl+" ^ (string_of_int (get_offset_of_const table (Sexpr sexpr2))) ^ ")"))
        in (add_to_table table constPair 17)
      | TaggedSexpr (s,sexpr1) -> let (table, offset) = (let constant = (const,(offset,make_string s)) in (add_to_table table constant ((9+String.length s))))
        in (constant_of_sexpr (Sexpr sexpr1) table offset)
      | TagRef s ->  let constant = (const,(offset,make_string s)) in (add_to_table table constant ((9+String.length s)))
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
    | Sexpr (TagRef _) -> 1 + 8 (*TagRef ???*)
    | Sexpr (Pair _) -> 1 + 8 + 8
    | Sexpr (TaggedSexpr (str, _)) (*TaggedSexpr ???*)
    | Sexpr (String str) -> 1 + 8 + String.length str
  ;;

  (* https://stackoverflow.com/a/19338726/7997683 *)
  let counterGenerator label =
    let count = ref (-1)  (*in the first time, inc is done and then returned*)
    in
    fun isToIncrease ->
      if isToIncrease
      then incr count;
      label ^ string_of_int !count (* this is outside the if expression, this is possible because the "incr" function returns unit *)
  ;;

  let label_Lelse_counter = counterGenerator "else"
  and label_Lexit_counter = counterGenerator "exit"
  and label_Lcode_counter = counterGenerator "code"
  and label_Lcont_counter = counterGenerator "cont"
  and label_CopyEnvLoop_counter = counterGenerator "copy_env_loop"
  and label_CopyParams_counter = counterGenerator "copy_params_loop"
  and label_AfterEnvCopy_counter = counterGenerator "after_env_copy"
  and label_MakeClosure_counter = counterGenerator "make_closure"
  and label_ApplicProcIsColusre = counterGenerator "applic_proc_is_colsure"
  ;;

  let get_offset_fvar table var =
    List.fold_left (fun index (s,off) -> if (index>(-1)) then index else (if (s=var) then off else index)) (-1) table
  ;;

  let rec generateRec consts fvars e envSize =
    match e with
    | Const' constant -> "mov rax, const_tbl + " ^ string_of_int (get_offset_of_const consts constant) ^ "\n"
    | Var' (VarFree s) -> "mov rax, [fvar_tbl+" ^ string_of_int (get_offset_fvar fvars s) ^ "]\n"
    | Seq' exprlist -> List.fold_left (fun acc expr' -> acc ^ generateRec consts fvars expr' envSize) "" exprlist
    (* very very important !!!!!
       the labels generator will evaluate in undefined order, so make sure by hand that all calls to counter are in the right order *)
    | If' (test, dit, dif) ->
      let elseLabelWithInc = label_Lelse_counter true
      and exitLabelWithInc = label_Lexit_counter true
      in   (*this is important because the generations of dit, dif can include these labels*)
      let elseLabel = label_Lelse_counter false
      and exitLabel = label_Lexit_counter false
      in
      generateRec consts fvars test envSize ^
      "cmp rax, SOB_FALSE_ADDRESS\n" ^
      "je " ^ elseLabelWithInc ^ "\n" ^
      generateRec consts fvars dit envSize ^
      "jmp " ^ exitLabelWithInc ^ "\n" ^
      elseLabel ^ ":\n" ^
      generateRec consts fvars dif envSize ^
      exitLabel ^ ":\n"
    | Or' exprlist ->
      let exitLabelWithInc = label_Lexit_counter true
      and exitLabel = label_Lexit_counter false
      in
      let first = generateRec consts fvars (List.hd exprlist) ^
                  "cmp rax, SOB_FALSE_ADDRESS\n" ^
                  "jne " ^ exitLabelWithInc ^ "\n"
      in
      let (acc, _) =
        List.fold_left (fun (acc,index) curr ->
            let newacc = generateRec consts fvars curr ^
                         if index > List.length exprlist
                         then exitLabel ^ ":\n"
                         else "cmp rax, SOB_FALSE_ADDRESS\n" ^
                              "jne " ^ exitLabel ^ "\n"
            in (acc ^ newacc, index + 1))
          (first, 2) exprlist
      in acc
    | Def' (Var' (VarFree s), expr) ->
      (generateRec consts fvars expr envSize)
      ^ "mov qword [fvar_tbl+" ^ string_of_int (get_offset_fvar fvars s) ^ "], rax\n"
      ^ "mov rax, SOB_VOID_ADDRESS\n"
    | Set' (Var' (VarFree s), expr) ->
      (generateRec consts fvars expr envSize)
      ^ "mov qword [fvar_tbl+" ^ string_of_int (get_offset_fvar fvars s) ^ "], rax\n"
      ^ "mov rax, SOB_VOID_ADDRESS\n"
    | LambdaSimple' (params, body) ->
      let copyEnvLoopWithInc = label_CopyEnvLoop_counter true
      and copyEnvLoopLabel = label_CopyEnvLoop_counter false
      and copyParamsLoopWithInc = label_CopyParams_counter true
      and copyParamsLoopLabel = label_CopyParams_counter false
      and codeLabelWithInc = label_Lcode_counter true
      and codeLabel = label_Lcode_counter false
      and contLabelWithInc = label_Lcont_counter true
      and contLabel = label_Lcont_counter false
      and afterEnvCopyWithInc = label_AfterEnvCopy_counter true
      and afterEnvCopyLabel = label_AfterEnvCopy_counter false
      and makeClosureWithInc = label_MakeClosure_counter true
      and makeClosureLabel = label_MakeClosure_counter false
      in
      let code =
        (* allocate new env, so rax <- the address to the extended env*)
        "MALLOC rax, WORD_SIZE * " ^ string_of_int (envSize + 1) ^ "\n" ^ (*rax <- address to ExtEnv*)
        (*copy env*)
        "mov rbx, [rbp + 2 * WORD_SIZE]\n" ^ (*now rbx holds the pointer to the previous env*)
        "mov rcx, " ^ string_of_int envSize ^"\n" ^
        "cmp rcx, 0\n" ^
        "jle " ^ afterEnvCopyWithInc ^ "\n" ^
        copyEnvLoopWithInc ^ ":\n" ^ (*rcx will go from n...1*)
        "\tmov rdx, [rbx + WORD_SIZE * rcx - WORD_SIZE]\n" ^
        "\tmov [rax + WORD_SIZE * rcx], rdx\n" ^
        "\tloop " ^ copyEnvLoopLabel ^ "\n" ^
        (* now we'll peform ExtEnv[0] -> vector with params *)
        afterEnvCopyLabel ^ ":\n" ^
        "mov rbx, rax\n" ^ (* rbx <- ExtEnv*)
        "mov rax, WORD_SIZE\n" ^
        "mov rcx, [rbp + 3 * WORD_SIZE]\n" ^  (* rcx<-n from the stack*)
        "mul rcx\n" ^ (* rax <- n*WORD_SIZE*)
        "MALLOC rdx, rax\n" ^ (* rdx <- address to new vector*)
        "mov [rbx], rdx\n" ^ (* ExtEnv[0] -> new vector *)
        "mov rcx, [rbp + 3 * WORD_SIZE]\n" ^  (* rcx<-n from the stack*)
        "cmp rcx, 0\n" ^
        "jle " ^ makeClosureWithInc ^ "\n" ^
        copyParamsLoopWithInc ^ ":\n" ^  (*rcx will go from n...1*)
        "\tmov rax, [rbp + 4 * WORD_SIZE + WORD_SIZE * rcx - WORD_SIZE]\n" ^ (* rax <- param(rcx-1) *)
        "\tmov [rdx + WORD_SIZE * rcx - WORD_SIZE], rdx\n" ^ (* new vector[rcx-1] <- param(rcx-1) *)
        "\tloop " ^ copyParamsLoopLabel ^ "\n" ^
        (* Allocate closure object *)
        (* Closure → Env ≔ ExtEnv *)
        (* Closure → Code ≔ Lcode *)
        makeClosureLabel ^ ": MAKE_CLOSURE(rax, rbx, " ^ codeLabelWithInc ^ ")\n"
      in
      code ^
      "jmp " ^ contLabelWithInc ^ "\n" ^
      (*Lcode label, this is a piece of code that is not executed now, just written for the closure*)
      codeLabel ^ ":\n" ^
      "\tenter 0, 0\n" ^
      generateRec consts fvars body (envSize + 1) ^
      "\tleave\n" ^
      "\tret\n" ^
      contLabel ^ ":\n"
    | Applic' (proc, args) ->
      let applicProcIsClosureWithInc = label_ApplicProcIsColusre true
      and applicProcIsClosure = label_ApplicProcIsColusre false
      in
      List.fold_left (fun acc arg -> acc ^ generateRec consts fvars arg envSize ^
                                     "push rax\n") "" args ^
      "push " ^ string_of_int (List.length args) ^ "\n" ^
      generateRec consts fvars proc envSize ^
      "cmp byte [rax], T_CLOSURE\n
      je " ^ applicProcIsClosureWithInc ^ "\n" ^
      (* what to do when proc is not a clousre *)
      "mov rax, 0\nadd rsp, 4*8\npop rbp\nret\n" ^
      (* what to do when proc is a closure*)
      applicProcIsClosure ^ ":
\tCLOSURE_ENV rbx, rax
\tpush rbx
\tCLOSURE_CODE rbx, rax
\tcall rbx
\tadd rsp, 8 * 1 ;pop env
\tpop rbx ;pop arg count
\tshl rbx, 3 ;rbx = rbx * 8
\tadd rsp, rbx ;pop args\n"
    | _ -> raise X_not_yet_implemented
  ;;

  (* creates assembly code for single expr', these strings will concat
     the prolog will contains the section .data init of const_tbl and freevar_tbl *)
  let generate consts fvars e = generateRec consts fvars e 0;;
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

(*let expr' = Def' (Var' (VarFree "a"), Const' (Sexpr (Number (Int 3))));;
  Code_Gen.make_fvars_tbl [expr'];;
*)
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

module Code_Gen (*: CODE_GEN*) = struct
  
  let tagsLst = ref [];;

  (*returns the sexpr Tagged(s,sexpr) in the list[(s,sexpr)]*)
  let getSexprOfTagged s =
    match List.find_opt (fun (name, sxpr) -> name = s) !tagsLst with
    | Some (_, sxpr) -> sxpr
    | None -> raise X_this_should_not_happen
    ;;

  (*return -1 if const is not in the table, otherwise returns the offset in the table*)
  let rec get_offset_of_const table const =
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
    let make_string s = ("MAKE_LITERAL_STRING \"" ^ s ^ "\"")
    in
    match const with
    | Void -> (table, offset)
    | Sexpr (sexpr) ->
      match sexpr with
      | Nil | Bool _ | TagRef _ -> (table, offset) (*these constants are already defined in the table*)
      | Number (Int i) -> let constant = (const,(offset,"MAKE_LITERAL_INT(" ^ (string_of_int i) ^ ")")) in (add_to_table table constant 9)
      | Number (Float f) -> let constant = (const,(offset,"MAKE_LITERAL_FLOAT(" ^ (string_of_float f) ^ ")")) in (add_to_table table constant 9)
      | Char c -> let constant = (const,(offset,"MAKE_LITERAL_CHAR('" ^ (String.make 1 c) ^ "')")) in (add_to_table table constant 2)
      | String s -> let constant = (const,(offset, make_string s)) in (add_to_table table constant (9+String.length s))
      (* maybe needed '\0' at end of string*)
      | Symbol s ->
        let offsetOfString = get_offset_of_const table (Sexpr (String s))
        in let (table,offsetAFTERString) = if (offsetOfString=(-1)) then constant_of_sexpr (Sexpr (String s)) table offset else (table,offsetOfString)
        in let constant = (const,(offsetAFTERString,"MAKE_LITERAL_SYMBOL(const_tbl + " ^ (string_of_int offset) ^ ")"))
        in (add_to_table table constant 9)
      | Pair (sexpr1,sexpr2) -> let (table, offset) = constant_of_sexpr (Sexpr (sexpr1)) table offset
        in let (table, offset) = constant_of_sexpr (Sexpr sexpr2) table offset
        in let constPair = (const, (offset, "MAKE_LITERAL_PAIR(const_tbl + " ^ (string_of_int (get_offset_of_const table (Sexpr sexpr1))) ^ ", const_tbl + " ^ (string_of_int (get_offset_of_const table (Sexpr sexpr2))) ^ ")"))
        in (add_to_table table constPair 17)
      | TaggedSexpr (s, sexpr1) ->
        (tagsLst := !tagsLst @ [(s, sexpr1)];
        constant_of_sexpr (Sexpr (sexpr1)) table offset)
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

  (*אחרי שיש את הטבלה אחרי המעבר הראשון, זה סבבה שיש שם -1 במקום טאג-רפ
  צריך מעבר שני, שיעבור על כל קונסט בטבלה, באופן רקורסיבי, ואם יש טאג-רפ הוא יחפש ברשימת הקסם אם יש כזה
  ואם יש כזה- הוא יחפש אותו בטבלת קבועים הקיימת, וייקח את האינדקס ששם ויופי והביתה :) *)
  let secondRound prevTable = 
    let rec replaceConst (const : constant) assembly = (* this will return new element to the const table : (const, (offset, assembly)) *) 
      match const with
      | Void -> assembly
      | Sexpr (sexpr) ->
        match sexpr with
        | Nil | Bool _ -> assembly (*these constants are already defined in the table*)
        | Number _| Char _| String _| Symbol _-> assembly
        | TagRef s -> string_of_int (get_offset_of_const prevTable (Sexpr (getSexprOfTagged s)))
        | Pair (sexpr1, sexpr2) ->
          let carString = 
          (match sexpr1 with 
          | TagRef s -> string_of_int (get_offset_of_const prevTable (Sexpr (getSexprOfTagged s)))
          | _ -> string_of_int (get_offset_of_const prevTable (Sexpr sexpr1)))
          and cdrString =
          (match sexpr2 with 
          | TagRef s -> string_of_int (get_offset_of_const prevTable (Sexpr (getSexprOfTagged s)))
          | _ -> string_of_int (get_offset_of_const prevTable (Sexpr sexpr2)))
          in
          "MAKE_LITERAL_PAIR(const_tbl + " ^ carString ^ ", const_tbl + " ^  cdrString ^ ")"
        | TaggedSexpr (s, sexpr1) -> raise X_this_should_not_happen (* taggedSexpr can't be in constant table!!!!*)
    in
    List.fold_left (fun newTable (const, (offset, assembly)) -> newTable@[(const, (offset, replaceConst const assembly))]) [] prevTable
    (* List.map (fun (const, (offset, assembly)) -> replaceConst const offset) prevTable *)
  ;;

  let make_consts_tbl asts =
    let (table, offset) = List.fold_left (fun (table,offset) ast -> (add_constants_to_table ast table offset))
        ([(Void, (0, "MAKE_VOID"));
          (Sexpr (Nil), (1, "MAKE_NIL"));
          (Sexpr (Bool false), (2, "MAKE_BOOL(0)"));
          (Sexpr (Bool true), (4, "MAKE_BOOL(1)"))] , 6)
        asts
    in
    secondRound table
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
    let procedures =
      ["boolean?", "is_boolean"; "float?", "is_float"; "integer?", "is_integer"; "pair?", "is_pair";
       "null?", "is_null"; "char?", "is_char"; "string?", "is_string";
       "procedure?", "is_procedure"; "symbol?", "is_symbol"; "string-length", "string_length";
       "string-ref", "string_ref"; "string-set!", "string_set"; "make-string", "make_string";
       "symbol->string", "symbol_to_string"; "char->integer", "char_to_integer"; "integer->char", "integer_to_char"; "eq?", "is_eq";
       "+", "bin_add"; "*", "bin_mul"; "-", "bin_sub"; "/", "bin_div"; "<", "bin_lt"; "=", "bin_equ";
       "apply", "apply"; "car", "car"; "cdr", "cdr"; "cons", "cons"; "set-car!", "set_car"; "set-cdr!", "set_cdr"]
    in
    let (table, offset) = List.fold_left (fun (table, offset) (proc, label) -> (table @ [(proc, offset)], offset + 8)) ([], 0) procedures
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
  and label_LambdaOptShrinkStack = counterGenerator "shrink_stack"
  and label_LambdaOptEnlargeStack = counterGenerator "enlarge_stack"
  and label_CopyArgs_counter = counterGenerator "copy_args_loop"
  and label_shrinkLoop_counter = counterGenerator "shrink_stack_loop"
  ;;

  let get_offset_fvar table var =
    List.fold_left (fun index (s,off) -> if (index>(-1)) then index else (if (s=var) then off else index)) (-1) table
  ;;

  let shrinkStack params optional body =
    let paramsLength = List.length params
    and copyArgsLabelWithInc = label_CopyArgs_counter true
    and copyArgsLabel = label_CopyArgs_counter false
    and shrinkLoopWithInc = label_shrinkLoop_counter true
    and shrinkLoopLabel = label_shrinkLoop_counter false
    in
    (* STEP1: create the optional list and puts it in rax*)
    "mov rcx, [rsp + 2 * WORD_SIZE] ;rcx=runtime n\n" ^
    "mov rbx, SOB_NIL_ADDRESS ;rbx='()\n" ^
    "sub rcx, " ^ string_of_int paramsLength ^ "\n" ^
    copyArgsLabelWithInc ^ ":\n" ^
    "\tmov r8, [rsp + 2 * WORD_SIZE + " ^ string_of_int paramsLength ^ " * WORD_SIZE + rcx * WORD_SIZE]\n" ^
    (*3 for ret,env,n, paramsLength, rcx to get the element of optional*)
    "\tMAKE_PAIR(rdx, r8, rbx)\n" ^
    "\tmov rbx, rdx\n" ^
    "\tloop " ^ copyArgsLabel ^ "\n" ^
    (* STEP2: put rax on the stack, override the first element of optional*)
    "mov [rsp + 2 * WORD_SIZE + " ^ string_of_int paramsLength ^ " * WORD_SIZE + WORD_SIZE], rbx\n" ^
    (* STEP3: move all the frame back with the appropriate offset *)
    (* rax will be the offset in the stack *)
    "mov rax, [rsp + 2*WORD_SIZE]\n" ^
    "sub rax, " ^ string_of_int paramsLength ^ "\n" ^
    "sub rax,1\n" ^
    "mov rcx, [rsp + 2*WORD_SIZE]\n" ^
    "add rcx, 1\n" ^ (* override *)
    "add rcx, 3\n" ^ (* rcx <- 3 (for ret,env,n) + paramsLength + 1*)
    shrinkLoopWithInc ^ ":\n" ^
    "\tmov rbx, [rsp + rcx * WORD_SIZE - WORD_SIZE]\n" ^
    "\tmov qword r9, rcx\n" ^
    "\tsub r9, 1\n" ^
    "\tadd r9, rax\n" ^ (* r9 <- rcx-1+rax (rax is the offset)*)
    "\tmov [rsp + r9 * WORD_SIZE ], rbx\n" ^
    "\tloop " ^ shrinkLoopLabel ^ "\n" ^
    (* important STEP *)
    "mov rbx, WORD_SIZE\n" ^
    "mul rbx\n" ^ (* rax <- offset * WORD_SIZE*)
    "add qword rsp, rax\n" ^
    (* STEP5: replace n <- n - paramsLength + 1*)
    "sub qword [rsp + 2 * WORD_SIZE], " ^ string_of_int paramsLength ^ "\n" ^
    "add qword [rsp + 2 * WORD_SIZE], 1\n"
  ;;

  (*return string of the code to adjust stack*)
  let adjust_stack params optional body =
    let shrinkStackLabelWithInc = label_LambdaOptShrinkStack true
    (* and shrinkStackLabel = label_LambdaOptShrinkStack false *)
    and enlargeStackLabelWithInc = label_LambdaOptEnlargeStack true
    and enlargeStackLabel = label_LambdaOptEnlargeStack false
    and contLabelEnlargeWithInc = label_Lcont_counter true
    and contLabelenlarge = label_Lcont_counter false
    and contLabelFINALWithInc = label_Lcont_counter true
    and contLabelFINAL = label_Lcont_counter false
    and copyArgsLabelWithInc = label_CopyArgs_counter true
    and copyArgsLabel = label_CopyArgs_counter false
    in
    let countparams = (List.length params)
    in
    "\tmov qword rbx, [rsp + WORD_SIZE * 2]\n" ^
    "\tcmp rbx, " ^ string_of_int countparams ^ "\n" ^ (*check if shrink or enlarge*)
    "\tje " ^ enlargeStackLabelWithInc ^ "\n" ^
    (*here comes the code to shrink stack*)
    shrinkStackLabelWithInc ^ ":\n" ^
    (shrinkStack params optional body) ^
    "\tjmp " ^ contLabelFINALWithInc ^ "\n" ^
    (*here comes the code to enlarge stack*)
    enlargeStackLabel ^ ":\n" ^
    "\tpop rax\n" ^ (*rax <- ret*)
    "\tpop rbx\n" ^ (*rbx <- env*)
    "\tpush " ^ string_of_int (countparams + 1) ^ "\n" ^
    "\tpush rbx\n" ^
    "\tpush rax\n" ^
    (*now the stack is: ret|env|n+1|n|n args*)
    "\tmov qword rcx, 1\n" ^ (*rcx goes 1...n*)
    copyArgsLabelWithInc ^ ":\n" ^
    "\tcmp rcx, " ^ string_of_int countparams ^ "\n" ^
    "\tjg " ^ contLabelEnlargeWithInc ^ "\n" ^  (* jmp greater for rcx=1, n=0*)
    "\tmov rbx, rcx\n" ^
    "\tadd rbx, 3\n" ^
    "\tmov rax, [rsp + rbx * WORD_SIZE]\n" ^ (*rax<-args(rcx)*)
    "\tsub rbx, 1\n" ^
    "\tmov [rsp + rbx * WORD_SIZE], rax\n" ^
    "\tadd rcx, 1\n" ^
    "\tjmp " ^ copyArgsLabel ^ "\n" ^
    contLabelenlarge ^ ":\n" ^
    (* code here to set the optional at the right place in stack*)
    "\tmov rax, SOB_NIL_ADDRESS\n" ^
    "\tadd rcx, 2\n" ^
    "\tmov [rsp + rcx * WORD_SIZE], rax\n" ^
    (*FINAL*)
    contLabelFINAL ^ ":\n"
  ;;

  let rec generateRec consts fvars e envSize =
    match e with
    | Const' constant -> 
      (match constant with
      | Sexpr (TagRef s) -> "mov rax, const_tbl + " ^ string_of_int (get_offset_of_const consts (Sexpr (getSexprOfTagged s))) ^ "\n"
      |  _ -> "mov rax, const_tbl + " ^ string_of_int (get_offset_of_const consts constant) ^ "\n")
    | Var' (VarParam (_, minor)) -> "mov rax, qword [rbp + WORD_SIZE * (4 + " ^ string_of_int minor ^ ")]\n"
    | Var' (VarFree s) -> "mov rax, [fvar_tbl + " ^ string_of_int (get_offset_fvar fvars s) ^ "]\n"
    | Var' (VarBound (_, major, minor)) ->
      "mov rax, qword [rbp + WORD_SIZE * 2]\n" ^
      "mov rax, qword [rax + WORD_SIZE * " ^ string_of_int major ^ "]\n" ^
      "mov rax, qword [rax + WORD_SIZE * " ^ string_of_int minor ^ "]\n"
    | BoxGet' var ->
      generateRec consts fvars (Var' var) envSize ^
      "mov rax, qword [rax]\n"
    | BoxSet' (var, e) ->
      generateRec consts fvars e envSize ^
      "push rax\n" ^
      generateRec consts fvars (Var' var) envSize ^
      "pop qword [rax]\n" ^
      "mov rax, SOB_VOID_ADDRESS\n"
    | Seq' exprlist -> List.fold_left (fun acc expr' -> acc ^ generateRec consts fvars expr' envSize) "" exprlist
    | Set' (Var'(VarParam(_, minor)), e) ->
      generateRec consts fvars e envSize ^
      "mov qword [rbp + WORD_SIZE * (4 + " ^ string_of_int minor ^ ")], rax\n" ^
      "mov rax, SOB_VOID_ADDRESS\n"
    | Set' (Var'(VarFree(v)), e) ->
      generateRec consts fvars e envSize ^
      "mov qword [" ^ string_of_int (get_offset_fvar fvars v) ^ "], rax" ^
      "mov rax, SOB_VOID_ADDRESS\n"
    | Set'( Var'(VarBound(_, major, minor)), e) ->
      (generateRec consts fvars e envSize) ^
      "mov rbx, qword [rbp + WORD_SIZE * 2]\n" ^
      "mov rbx, qword [rbx + WORD_SIZE * " ^ string_of_int major ^ "]\n" ^
      "mov qword [rbx + WORD_SIZE * " ^ string_of_int minor ^ "], rax\n" ^
      "mov rax, SOB_VOID_ADDRESS\n"
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
      let first = generateRec consts fvars (List.hd exprlist) envSize ^
                  "cmp rax, SOB_FALSE_ADDRESS\n" ^
                  "jne " ^ exitLabelWithInc ^ "\n"
      in
      let (acc, _) =
        List.fold_left (fun (acc,index) curr ->
            let newacc = (generateRec consts fvars curr envSize) ^
                         if index > List.length exprlist
                         then exitLabel ^ ":\n"
                         else "cmp rax, SOB_FALSE_ADDRESS\n" ^
                              "jne " ^ exitLabel ^ "\n"
            in (acc ^ newacc, index + 1))
          (first, 2) exprlist
      in acc
    | Def' (Var' (VarFree s), expr) ->
      generateRec consts fvars expr envSize ^
      "mov qword [fvar_tbl + " ^ string_of_int (get_offset_fvar fvars s) ^ "], rax\n" ^
      "mov rax, SOB_VOID_ADDRESS\n"
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
        "\tmov [rdx + WORD_SIZE * rcx - WORD_SIZE], rdx\n" ^ (* new vector[rcx - 1] <- param(rcx-1) *)
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
      "\tpush rbp\n" ^
      "\tmov rbp, rsp\n" ^
      generateRec consts fvars body (envSize + 1) ^
      "\tleave\n" ^
      "\tret\n" ^
      contLabel ^ ":\n"
    | LambdaOpt' (params, optional, body) ->
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
        "\tmov [rdx + WORD_SIZE * rcx - WORD_SIZE], rdx\n" ^ (* new vector[rcx - 1] <- param(rcx-1) *)
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
      (*Adjust stack for opt args*)
      (adjust_stack params optional body) ^
      "\tpush rbp\n" ^
      "\tmov rbp, rsp\n" ^
      generateRec consts fvars body (envSize + 1) ^
      "\tleave\n" ^
      "\tret\n" ^
      contLabel ^ ":\n"
    | Applic' (proc, args) ->
      let applicProcIsClosureWithInc = label_ApplicProcIsColusre true
      and applicProcIsClosure = label_ApplicProcIsColusre false
      in
      List.fold_right (fun arg acc -> acc ^
                                      generateRec consts fvars arg envSize ^
                                      "push rax\n") args "" ^
      "push " ^ string_of_int (List.length args) ^ "\n" ^
      generateRec consts fvars proc envSize ^
      "cmp byte [rax], T_CLOSURE\n
      je " ^ applicProcIsClosureWithInc ^ "\n" ^
      (* what to do when proc is not a clousre *)
      "mov rax, 0\n" ^
      "add rsp, 4 * WORD_SIZE\n" ^
      "pop rbp\n" ^
      "ret\n" ^
      (* what to do when proc is a closure*)
      applicProcIsClosure ^ ":\n" ^
      "\tCLOSURE_ENV rbx, rax\n" ^
      "\tpush rbx\n" ^
      "\tCLOSURE_CODE rbx, rax\n" ^
      "\tcall rbx\n" ^
      "\tadd rsp, WORD_SIZE * 1   ;pop env\n" ^
      "\tpop rbx          ;pop arg count\n" ^
      "\tshl rbx, 3       ;rbx = rbx * 8\n" ^
      "\tadd rsp, rbx     ;pop args\n"
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
Code_Gen.make_consts_tbl (List.map Semantics.run_semantics
                            (Tag_Parser.tag_parse_expressions
                               (Reader.read_sexprs "(define x '#{a}=2)
                               (define y '(#{a}=#t #{a}))")));;
!Code_Gen.tagsLst;;
open Ast

module StringMap = Map.Make(String)

let analyze (globals, functions) = 

  (* Raise an exception if the given list has a duplicate frames *)
  let report_duplicate exceptf list = 
    let rec helper = function
      | n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      |  _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
    | (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  (* Raise an excp if given rvalue type can't be assigned to the given lvalue type *)
  let check_assign lvaluet rvaluet err =  
    if lvaluet == rvaluet then lvaluet 
    else raise err
  in

  (* Returns the global var decl list   *)
  let get_var_decl globs = 
    (List.map (fun (x,y) -> y) globs.var_decls) 
  in 

  (* Check for restricted function names *)
  if List.mem "print" (List.map (fun fd -> fd.fname) functions) then 
    raise (Failure ("function print may not be defined")) 
  else ();
  if List.mem "build" (List.map (fun fd -> fd.fname) functions) then 
    raise (Failure ("function build may not be defined")) 
  else ();
  if List.mem "join" (List.map (fun fd -> fd.fname) functions) then 
    raise (Failure ("function join may not be defined")) 
  else ();
  if List.mem "convert" (List.map (fun fd -> fd.fname) functions) then 
    raise (Failure ("function convert may not be defined")) 
  else ();

  (* Check for duplicate function names *)
  report_duplicate (fun n -> "duplicate function " ^ n) (List.map (fun fd -> fd.fname) functions);

  (* Function declarations for built-in functions *)
  let built_in_decls = 
    StringMap.add "Join"
      { typ     = Void; 
        fname   = "Join";    
        formals = [ (Frame ({ x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""}), "A"); 
                    (FaceId({ dim = (0,0,0); face = ""; fc_id = ""}), "B"); 
                    (Frame ({ x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""}), "C"); 
                    (FaceId({ dim = (0,0,0); face = ""; fc_id = ""}), "D"); ];
        body    = [] };
    StringMap.add "Build"
      { typ     = Void; 
        fname   = "Build";
        formals = [ (Frame ({ x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""}), "A"); 
                    (FaceId({ dim = (0,0,0); face = ""; fc_id = ""}), "B"); 
                    (Frame ({ x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""}), "C"); 
                    (FaceId({ dim = (0,0,0); face = ""; fc_id = ""}), "D"); ];
        body    = [] };
    StringMap.add "Convert"
      { typ     = Void; 
        fname   = "Convert"; 
        formals = [(Frame ({ x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""}), "A")]; 
        body    = [] };
    StringMap.add "print"
      { typ     = Void; 
        fname   = "print";   
        formals = [(String, "s")];  
        body    = [] } 
    (StringMap.singleton "print_bool"
      { typ     = Void; 
        fname   = "print_bool";  
        formals = [(Bool, "b")];    
        body    = [] });
    (StringMap.singleton "print_int"
      { typ     = Void; 
        fname   = "print_int";  
        formals = [(Int, "i")];     
        body    = [] });
    (StringMap.singleton "print_flt"
      { typ     = Void; 
        fname   = "print_flt";  
        formals = [(Float, "f")];     
        body    = [] });
  in
  
  (* Add built-in function declarations to map, mapping: func_name -> func_decl *)
  let function_decls = 
    List.fold_left (fun map fdecl -> StringMap.add fdecl.fname fdecl map) built_in_decls functions
  in

  (* Check for unrecognized functions *)
  let function_decl s = try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  (* Check for main function *)
  let check_main_decl = try StringMap.find "main" function_decls
    with Not_found -> raise (Failure ("missing main() entry point"))
  in


  (* Check globals for dup var/frame/face decls - NOT WORKING CORRECTLY *)
  let check_globals glob =
    report_duplicate (fun n -> "duplicate global var/frame/face decl" ^ n) 
      (List.rev (get_var_decl glob)) 
  in
  List.iter check_globals globals;


  (* Check functions *)
  let check_functions func glob =

    (* Returns the global var decl list   *)
    let get_var_decl t = 
      (List.map (fun (x,y) -> y) t.var_decls) 
    in 

    (* Create a symbol table of globals, formals, and locals - NOT COMPLETE *)
    let symbols = List.fold_left (fun map (t, n) -> StringMap.add n t map)
      StringMap.empty ( glob @ func.formals ) (*@ func.body)*)
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (*((check_not_void 
      (fun n -> "illegal void argument" ^ n ^ " to function " ^ func.fname)) func.formals); *)

    (* report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals); *)

    (* List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals; *)

    (* report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals); *)

    (* Return expression type *)
    let rec check_expr = function
      | Id s       -> type_of_identifier s
      | Lit_Int  _ -> Int
      | Lit_Flt  _ -> Float
      | Lit_Bool _ -> Bool
      | Lit_Str  _ -> String
      | Null     _ -> Void
      | Binop(e1, op, e2) as e -> 
          let t1 = check_expr e1 and t2 = check_expr e2 
          in
          (match op with
            | Add   | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
            | Equal | Neq when t1 = t2 -> Bool
            | Less  | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
            | And   | Or when t1 = Bool && t2 = Bool -> Bool
            | _ -> raise (Failure ("illegal binary operator " ^
                  string_of_dtype t1 ^ " "    ^ string_of_op op ^ " " ^
                  string_of_dtype t2 ^ " in " ^ string_of_expr e)))
      | Unop(op, e) as ex -> 
          let t = check_expr e 
          in
          (match op with
            | Neg when t = Int -> Int
            | Not when t = Bool -> Bool
            | _ -> raise (Failure ("illegal unary operator " ^ 
                                    string_of_uop op  ^
                                    string_of_dtype t ^ " in " ^ 
                                    string_of_expr ex)))
      | Noexpr -> Void
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier var and rt = check_expr e 
          in
          check_assign lt rt (Failure ("illegal assignment " ^
                                        string_of_dtype lt ^ " = " ^ 
                                        string_of_dtype rt ^ " in " ^ 
                                        string_of_expr ex))
      | Call(fname, actuals) as call -> 
          let fd = function_decl fname 
          in
          if List.length actuals != List.length fd.formals then
            raise (Failure ("expecting "     ^ string_of_int (List.length fd.formals) ^ 
                            " arguments in " ^ string_of_expr call))
          else
            List.iter2 (fun (ft, _) e -> 
                  let et = check_expr e 
                  in
                  ignore (check_assign ft et
                  (Failure ("illegal actual argument found " ^ string_of_dtype et ^ " expected " ^
                            string_of_dtype ft ^ " in " ^ string_of_expr e)))) 
    fd.formals actuals;
    fd.typ
    in

    let check_bool_expr e = 
      if check_expr e != Bool then 
        raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
      else () 
    in

    let check_frame_stmt = function
      | Frame({ x; y; z; blocks; fr_id}) -> true
      | _ -> false 
    in


    (* Check statements *)
    let rec check_stmt = function
      | Print e   -> ignore (check_expr e)
      (* | Convert f -> ignore (check_frame_stmt f) *)
          (* let t = check_frame_stmt f in 
          if t  then () 
          else raise (Failure ("return gives " ^ string_of_dtype t ^ 
                              " expected " ^ string_of_dtype func.typ ^ 
                              " in " ^ string_of_expr f)) *)
      | If(p, b1, b2)       -> check_bool_expr p; check_stmt b1; check_stmt b2
      | Block sl  -> 
          let rec check_block = function
            | [Return _ as s] -> check_stmt s
            | Return _ :: _   -> raise (Failure "statements after return")
            | Block sl :: ss  -> check_block (sl @ ss)
            | s :: ss -> check_stmt s ; check_block ss
            | [] -> ()
          in check_block sl
      | Expr e   -> ignore (check_expr e)
      | Break    -> ()
      | Continue -> ()
      | Return e -> 
          let t = check_expr e 
          in 
          if t = func.typ then () 
          else raise (Failure ("return gives " ^ string_of_dtype t ^ 
                              " expected " ^ string_of_dtype func.typ ^ 
                              " in " ^ string_of_expr e))
      | If(p, b1, b2)       -> check_bool_expr p; check_stmt b1; check_stmt b2
      | For(e1, e2, e3, st) -> ignore (check_expr e1); check_bool_expr e2;
                               ignore (check_expr e3); check_stmt st
      | While(p, s)         -> check_bool_expr p; check_stmt s
    in
    check_stmt (Block func.body)
  in
  List.iter check_functions functions globals 
  
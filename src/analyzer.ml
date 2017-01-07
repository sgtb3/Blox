open Ast
open Pprint
open Printf

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

  (* Raise excp if given rvalue type can't be assigned to given lvalue type *)
  let check_assign lvaluet rvaluet err =  
    if lvaluet == rvaluet then lvaluet 
    else raise err
  in
 
  (* Check func names against list of built-in funcs *)
  let redefined_func_list = 
    List.filter 
      (fun fname -> List.mem fname 
                    ["print"; "printb"; "printi"; "printfl"; 
                     "build"; "join"; "convert";])
        (List.map (fun fd -> fd.fname) functions) 
  in

  if List.length redefined_func_list != 0 then 
    raise (Failure ("illegal redefinition of built-in function(s): " ^ 
                      (String.concat " " (List.rev redefined_func_list)))) 
  else ();

  (* Check for duplicate function names *)
  report_duplicate 
    (fun n -> "duplicate function: " ^ n) 
      (List.map (fun fd -> fd.fname) functions);
  
  (* Create an empty map *)
  let m = StringMap.empty in

  (* Define built-in funcs *)
  let add_join m = StringMap.add "Join"
    { typ     = Void;
      fname   = "Join";    
      formals = [(Frame ({x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""}),"A"); 
                 (FaceId({dim = (0,0,0); face = ""; fc_id = ""}), "B");     
                 (Frame ({x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""}),"C"); 
                 (FaceId({dim = (0,0,0); face = ""; fc_id = ""}), "D");];
      body    = [] } m
  in 
  let add_build m = StringMap.add "Build"
    { typ     = Void; 
      fname   = "Build";
      formals = [(Frame ({x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""}),"A"); 
                 (FaceId({dim = (0,0,0); face = ""; fc_id = ""}), "B"); 
                 (Frame ({x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""}),"C"); 
                 (FaceId({dim = (0,0,0); face = ""; fc_id = ""}), "D");];
      body    = [] } m
  in
  let add_convert m = StringMap.add "Convert"
    { typ     = Void; 
      fname   = "Convert"; 
      formals = [(Frame({x = 0; y = 0; z = 0; blocks = [||]; fr_id = ""}),"A")]; 
      body    = [] } m
  in
  let add_print m = StringMap.add "print"
    { typ = Void; fname = "print"; formals = [(String, "s")]; body = [] } m
  in 
  let add_printb m = StringMap.add "printb"
    { typ = Void; fname = "printb"; formals = [(Bool, "b")]; body = [] } m 
  in 
  let add_printi m = StringMap.add "printi"
    { typ = Void; fname = "printi"; formals = [(Int, "i")]; body = [] } m
  in
  let add_printfl m = StringMap.add "printfl"
    { typ = Void; fname = "printfl"; formals = [(Float, "f")]; body = [] } m
  in

  (* Create map for built-in funcs *)
  let add_built_in_decls = 
    add_join 
      (add_build 
        (add_convert 
          (add_printfl (add_printi (add_printb (add_print m))))))
  in
  
  (* Add built-in func decls to StringMap; mapping: func_name -> func_decl *)
  let function_decls = 
    List.fold_left 
      (fun map fdecl -> StringMap.add fdecl.fname fdecl map) 
        add_built_in_decls functions
  in

  (* Check for unrecognized functions *)
  let function_decl fname = 
    try StringMap.find fname function_decls 
    with Not_found -> raise (Failure ("unrecognized function '" ^ fname ^ "'"))
  in

  (* Check for main function *)
  let _ = try StringMap.find "main" function_decls
    with Not_found -> raise (Failure ("missing main() entry point"))
  in
  
  (* Returns the string identifer for a global *)
  let get_glob_id globs = 
    (List.map (fun f -> match f with 
                | VarDecl(_,id)     -> id 
                | VarAssign(_,id,_) -> id
                | FrAssign(id,_)    -> id
                | FcAssign(id,_)    -> id
                | _ -> "") globals)
  in

  (* Check for duplicate global declarations *)
  let check_globals glob =
    report_duplicate (fun n -> "duplicate global declaration '" ^ n ^ "'") 
      (List.rev (get_glob_id glob)) 
  in
  check_globals globals; (* could have used List.iter since globals is a list *)


  (* Check functions *)
  let check_functions func =

    (* Create a list of globals (type, id) from globals list *)
    let global_bindings = 
      let rec make_type_id_mapping = function
        | []       -> []
        | hd :: tl -> (match hd with 
                        | VarDecl(dt,id)       -> [(dt,id)]
                        | VarAssign(dt,id,exp) -> [(dt,id)]
                        | _                    -> []) 
                       @ make_type_id_mapping (List.filter ((<>) hd) tl)
      in 
      make_type_id_mapping globals;
    in

    (* Create a list of locals (type, id) from func body *)
    let local_bindings = 
      let rec make_type_id_mapping = function
        | []       -> []
        | hd :: tl -> (match hd with 
                        | Var_Decl(dt,id)    -> [(dt,id)]
                        | Var_Assign(va,exp) -> [(fst va,snd va)]
                        | _                  -> []) 
                       @ make_type_id_mapping (List.filter ((<>) hd) tl)
      in 
      make_type_id_mapping func.body;
    in

    (* Check for void args in func definitions *)
    List.iter 
      (check_not_void 
        (fun n -> "illegal void formal argument '" ^ n ^ 
                  "' in function " ^ func.fname)) func.formals;

    (* Check for duplicate formal args in func definitions *)
    report_duplicate 
      (fun n -> "duplicate formal argument: '" ^ n ^ 
                "' in function " ^ func.fname) (List.map snd func.formals);

    (* Check for illegal void type declarations and declaration assignments *)
    List.iter 
      (check_not_void
        (fun n -> "illegal void local var decl '" ^ n ^ 
          "' in function " ^ func.fname)) local_bindings;

    let local_ids_list =
      let rec get_ids = function
        | [] -> []
        | (dt,id) :: tl -> id :: get_ids tl
      in
      get_ids local_bindings;
    in 

    (* Check for duplicate locals *)
    report_duplicate 
      (fun n -> "duplicate local '" ^ n ^ "' in function " ^ func.fname) 
        local_ids_list;

    (* Create a symbol table of globals, formals, and locals - NOT COMPLETE *)
    let symbols = 
      List.fold_left 
        (fun map (dt, id) -> StringMap.add id dt map)
          StringMap.empty (global_bindings @ func.formals @ local_bindings) 
          (* StringMap.empty (global_bindings @ func.formals @ local_bindings)  *)
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* testing - delete when symbol table works *)
    print_string "\n";

(* 
    
    (* Return expression type *)
    let rec check_expr = function
      | Id s       -> type_of_identifier s
      | Lit_Int  _ -> Int
      | Lit_Flt  _ -> Float
      | Lit_Bool _ -> Bool
      | Lit_Str  _ -> String
      | Null       -> Void
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
            | s :: ss         -> check_stmt s ; check_block ss
            | []              -> ()
          in 
          check_block sl
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
    check_stmt (Block func.body);  *) 
  in
  List.iter check_functions functions
  
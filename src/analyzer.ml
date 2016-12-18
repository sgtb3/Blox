open Ast
open Sast

module StringMap = Map.Make(String)

let analyze (globals, functions) = 

  (* print_endline "\n" *)

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

  (* (* Returns the frame name  *)
  let get_frname { fr_x=_; fr_y=_; fr_z=_; fr_name=_} = function 
    { fr_x=_; fr_y=_; fr_z=_; fr_name=fn } -> fn 
  in *)

  (* Returns the global var decl list version 2 *)
  let get_var_decl glob_vd = 
    (List.map (fun (x,y) -> y) glob_vd.var_decls) 
  in 

  (* Returns the global var decl list *)
  let get_var_decl_list { var_decls=_; var_assgns=_; fr_assgns=_} = 
    function { var_decls=vd_list; var_assgns=_; fr_assgns=_ } -> vd_list 
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
    (* NOT WORKING *)
    (* StringMap.add "Join"
      { typ     = Void; 
        fname   = "Join";    
        formals = [ (Frame(Ast.frame), "A"); (FaceId(Int,Int,Int,String), "B"); 
                    (Frame(Int,Int,Int), "C"); (FaceId(Int,Int,Int,String), "D"); ];
        body    = [] }; *)
    (* NOT WORKING *)
    (* StringMap.add "Build"
      { typ     = Void; 
        fname   = "Build";
        (* this needs to be Build((Frame(Int,Int,Int), "A", Frame(Int,Int,Int), "B") *) 
        formals = [(Build("frA","faceArrA","frB","faceArrB"), "f")];   
        body    = [] }; *)
    (* NOT WORKING *)
   (*  StringMap.add "Convert"
      { typ     = Void; 
        fname   = "Convert"; 
        formals = [(Frame(Int,Int,Int), "A")]; 
        body    = [] }; *)
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
  
  (* Add built-in function declarations to map *)
  let function_decls = 
    List.fold_left (fun m fd -> StringMap.add fd.fname fd m) built_in_decls functions
  in

  (* Unrecognized functions *)
  let function_decl s = try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  (* Missing main function *)
  let check_main_decl s = try StringMap.find s function_decls
    with Not_found -> raise (Failure ("missing main() entry point"))
  in

  (* Ensure "main" is defined *)
  let _ = check_main_decl "main" 
  in 


  (* Iterate over the list of globals *)
  let check_globals g =

    (* Check for duplicate global variable declarations - NOT WORKING CORRECTLY *)
    report_duplicate 
      (fun n -> "duplicate global var decl" ^ n) 
        (List.rev (get_var_decl g)) 

    (* Check for duplicate global frame declarations - NOT WORKING*)
    (* report_duplicate 
      (fun n -> "duplicate global frame decl" ^ n) 
        (List.rev (get_fr_decl g))  *)

    (* Check for duplicate global face declarations - NOT WORKING*)
    (* report_duplicate 
      (fun n -> "duplicate global face decl" ^ n) 
        (List.rev (get_fc_decl g))  *)

(* type globals = {
  var_decls  : var_decl list;
  var_assgns : var_assign list;
  fr_assgns  : fr_assign list;
  fc_assgns  : fc_assign list;
} *)

    (* (* Type of each variable (global, formal, or local *)
     let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
      StringMap.empty (g @ g.var_decls @g.var_assgns @ g.fr_assgns @ g.fc_assgns)
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in *)

  (*
    let check_bool_expr e = 
      if expr e != Bool then 
        raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
     else () 
    in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
      | Block sl -> let rec check_block = function
                      | [Return _ as s] -> stmt s
                      | Return _ :: _   -> raise (Failure "nothing may follow a return")
                      | Block sl :: ss  -> check_block (sl @ ss)
                      | s :: ss -> stmt s ; check_block ss
                      | [] -> ()
                    in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e 
                    in 
                    if t = func.typ then () 
                    else raise (Failure ("return gives " ^ string_of_typ t ^ 
                                      " expected " ^ string_of_typ func.typ ^ 
                                      " in " ^ string_of_expr e))
      | If(p, b1, b2)       -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s)         -> check_bool_expr p; stmt s
    in
    stmt (Block func.body) 
  *)

  in
  List.iter check_globals globals;
    

  
  (* Check functions*)
  let check_functions f =

      print_endline "Analyzer check_functions ... \n"

      (* (* Check for void formal arguments *)
      List.iter 
        (check_not_void (fun n -> "illegal void argument" ^ n ^ " to function " ^ f.fname)) 
            f.formals;

      report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ f.fname)
        (List.map snd f.formals);

      List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
        " in " ^ f.fname)) f.locals;

      report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ f.fname)
        (List.map snd f.locals); *)


     (*  (* Type of each variable (global, formal, or local *)
      let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
        StringMap.empty (globals @ f.formals @ f.locals )
      in

      let type_of_identifier s =
        try StringMap.find s symbols
        with Not_found -> raise (Failure ("undeclared identifier " ^ s))
      in

      (* Return the type of an expression or throw an exception *)
      let rec expr = function
        | Literal _ -> Int
        | BoolLit _ -> Bool
        | Id s -> type_of_identifier s
        | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 
                                    in
                                    (match op with
                                      | Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
                                      | Equal | Neq when t1 = t2 -> Bool
                                      | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
                                      | And | Or when t1 = Bool && t2 = Bool -> Bool
                                      | _ -> raise (Failure ("illegal binary operator " ^
                                            string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                                            string_of_typ t2 ^ " in " ^ string_of_expr e)))
        | Unop(op, e) as ex -> let t = expr e 
                               in
                               (match op with
                                 | Neg when t = Int -> Int
                                 | Not when t = Bool -> Bool
                                 | _ -> raise (Failure ("illegal unary operator " ^ 
                                                        string_of_uop op ^
                                                        string_of_typ t ^ " in " ^ 
                                                        string_of_expr ex)))
        | Noexpr -> Void
        | Assign(var, e) as ex -> let lt = type_of_identifier var and rt = expr e 
                                  in
                                  check_assign lt rt (Failure ("illegal assignment " ^
                                                                string_of_typ lt ^ " = " ^ 
                                                                string_of_typ rt ^ " in " ^ 
                                                                string_of_expr ex))
        | Call(fname, actuals) as call -> let fd = function_decl fname 
                                          in
                                          if List.length actuals != List.length fd.formals then
                                            raise (Failure ("expecting " ^ 
                                                            string_of_int (List.length fd.formals) ^ 
                                                            " arguments in " ^ string_of_expr call))
                                          else
                                            List.iter2 (fun (ft, _) e -> let et = expr e 
                                                                         in
                                                                         ignore (check_assign ft et
                                                                         (Failure ("illegal actual argument found " ^ 
                                                                                   string_of_typ et ^ " expected " ^
                                                                                   string_of_typ ft ^ " in " ^ 
                                                                                   string_of_expr e)))) 
      fd.formals actuals;
      fd.typ
      in

      let check_bool_expr e = 
        if expr e != Bool then 
          raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
       else () 
      in

      (* Verify a statement or throw an exception *)
      let rec stmt = function
        | Block sl -> let rec check_block = function
                        | [Return _ as s] -> stmt s
                        | Return _ :: _   -> raise (Failure "nothing may follow a return")
                        | Block sl :: ss  -> check_block (sl @ ss)
                        | s :: ss -> stmt s ; check_block ss
                        | [] -> ()
                      in check_block sl
        | Expr e -> ignore (expr e)
        | Return e -> let t = expr e 
                      in 
                      if t = func.typ then () 
                      else raise (Failure ("return gives " ^ string_of_typ t ^ 
                                        " expected " ^ string_of_typ func.typ ^ 
                                        " in " ^ string_of_expr e))
        | If(p, b1, b2)       -> check_bool_expr p; stmt b1; stmt b2
        | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                                 ignore (expr e3); stmt st
        | While(p, s)         -> check_bool_expr p; stmt s
      in
      stmt (Block func.body) *)

    in
    List.iter check_functions functions

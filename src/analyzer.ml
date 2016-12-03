open Ast
open Sast

module StringMap = Map.Make(String)

let run (globals, functions) =              
  print_endline "Skipping Analyzer...\n"
(* 
  let report_duplicate exceptf list = (* Raise an exception if the given list has a duplicate frames *)
    let rec helper = function
	     n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  let check_assign lvaluet rvaluet err =  (* Raise an excp if given rvalue type can't be assigned to the given lvalue type *)
    if lvaluet == rvaluet then 
      lvaluet 
    else 
      raise err
  in

  let rec fr_type = function 
    Fr_decl(_,_,_,name) -> name
  in

  report_duplicate (fun n -> "duplicate frame " ^ n) (List.map fr_type stmts); *)

  (* let built_in_decls =            (* Function declaration for a named function *)
    StringMap.add "print" { 
      fname = "print"; 
      formals = [(Int, "x")];
      locals = []; 
      body = [] 
    } (StringMap.singleton "printb" { 
          typ = Void; 
          fname = "printb"; 
          formals = [(Bool, "x")];
          locals = []; 
          body = [] })
  in
     
  let function_decls = 
    List.fold_left (fun m fd -> StringMap.add fd.fname fd m) built_in_decls functions
  in 

  let function_decl s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
 
  let _ = function_decl "main"      (* Ensure "main" is defined *)
  in 
  let check_function func =
    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^ " in " ^ func.fname)) func.formals;
    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname) (List.map snd func.formals);

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^ " in " ^ func.fname)) func.locals;
    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname) (List.map snd func.locals);

    (* Type of each variable (global, formal, or local *)
    let symbols = 
      List.fold_left (fun m (t, n) -> StringMap.add n t m) StringMap.empty (globals @ func.formals @ func.locals)
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
				Literal _ -> Int
      | Id s 			-> type_of_identifier s
      | Binop(e1, op, e2) as e -> 
          let t1 = expr e1 and t2 = expr e2 
          in
      	  (match op with 
              Add 
            | Sub 
            | Mult 
            | Div 
                when t1 = Int && t2 = Int   -> Int  | Equal | Neq 
                when t1 = t2                -> Bool | Less  | Leq | Greater | Geq 
                when t1 = Int && t2 = Int   -> Bool | And   | Or 
                when t1 = Bool && t2 = Bool -> Bool
            | _ -> raise (Failure ("illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in " ^ string_of_expr e)))
      | Unop(op, e) as ex -> 
          let t = expr e 
          in
      	  (match op with
      	      Neg when t = Int -> Int
      	    | Not when t = Bool -> Bool
            | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^ string_of_typ t ^ " in " ^ string_of_expr ex)))

      | Noexpr -> Void
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier var and rt = expr e 
          in
          check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex))
      | Call(fname, actuals) as call -> 
          let fd = function_decl fname 
          in
          if List.length actuals != List.length fd.formals then
            raise (Failure ("expecting " ^ string_of_int (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
          else
            List.iter2 (fun (ft, _) e -> 
              let et = expr e 
              in
              ignore (check_assign ft et (Failure ("illegal actual argument found " ^ string_of_typ et ^ " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
          fd.formals actuals;
          fd.typ
    in

    let check_bool_expr e = 
      if expr e != Bool then 
        raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
      else 
        () 
    in

    let rec stmt = function Block sl ->                  (* Verify a statement or throw an exception *)
      let rec check_block = function
          [Return _ as s] -> stmt s
        | Return _ :: _   -> raise (Failure "nothing may follow a return")
        | Block sl :: ss  -> check_block (sl @ ss)
        | s :: ss         -> stmt s ; check_block ss
        | []              -> ()
      in check_block sl
    | Expr e -> ignore (expr e)
    | Return e -> 
      let t = expr e 
      in 
      if t = func.typ then 
        ()
      else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^ string_of_typ func.typ ^ " in " ^ string_of_expr e))
    | If(p, b1, b2)       ->  check_bool_expr p; 
                              stmt b1; 
                              stmt b2
    | For(e1, e2, e3, st) ->  ignore (expr e1); 
                              check_bool_expr e2;
                              ignore (expr e3); 
                              stmt st
    | While(p, s) -> check_bool_expr p; stmt s
    in
    stmt (Block func.body) 
  in
  List.iter check_function functions


let get_arr_call_ret (thistype : typ) fname expr_types = match thistype with    (*get the return type of array, fail if not ok*)
  | Array x -> 
    let expr_len = List.length expr_types
    in
    begin match fname with
      | "push_back" ->
        if expr_len = 1 then
            if [x] = expr_types then 
              Void
            else 
              failwith ("type not consistent: get_arr_call_ret")
        else
            failwith ("push_back not 1 element: get_arr_call_ret")
      | "push_vec" ->
        if expr_len = 1 then
          let y = List.hd expr_types
          in
          match y with
          | Array z -> 
            if x = z then 
              Void
            else 
              failwith ("type not consistent: get_arr_call_ret")
          | _ -> failwith ("type not consistent: get_arr_call_ret")
        else
          failwith ("push_vec not 1 element: get_arr_call_ret")
      | "get_at" ->
        if expr_len = 1 then
          if [Int] = expr_types then 
            x
          else 
            failwith ("type not consistent: get_arr_call_ret")
        else
            failwith ("get_at not 1 element: get_arr_call_ret")
      | "set_at" ->
        if expr_len = 2 then
          if [Int;x] = expr_types then 
            Void
          else 
            failwith ("type not consistent: get_arr_call_ret")
        else
            failwith ("get_at not 1 element: get_arr_call_ret")
      | "size" ->
        if expr_len = 0 then
          Int
        else
            failwith("size should 0 element: get_arr_call_ret")
      | "sync" ->
        if expr_len = 0 then
          Void
        else
          failwith("sync should 0 element: get_arr_call_ret")
      | _ ->
        failwith ("not support build in array function")
    end
  | _ -> 
    failwith ("not array error")




(*get the return type of map, fail if not ok*)
let get_map_call_ret (thistype : typ) fname expr_types = match thistype with
  | Map (x,y) ->
    let expr_len = List.length expr_types
    in
    begin match fname with
      | "insert" ->
        if expr_len = 2 then
          if [x;y] = expr_types then 
            Void
          else 
            failwith ("type not consistent: get_map_call_ret")
        else
          failwith ("insert not 2 element: get_map_call_ret")
      | "get" ->
        if expr_len = 1 then
          if [x] = expr_types then 
            y
          else 
            failwith ("type not consistent: get_map_call_ret")
        else
            failwith ("get_at not 1 element: get_map_call_ret")
      | "size" ->
        if expr_len = 0 then
          Int
        else
          failwith("size should 0 element: get_map_call_ret")
      | "delete" ->
        if expr_len = 1 then
          if [x] = expr_types then 
            Void
          else 
            failwith ("type not consistent: get_map_call_ret")
        else
            failwith("delete should 1 element: get_map_call_ret")
      | "exist" ->
        if expr_len = 1 then
          if [x] = expr_types then 
            Bool
          else 
            failwith ("type not consistent: get_map_call_ret")
        else
            failwith("exist should be 1 element: get_map_call_ret")
      | "sync" ->
        if expr_len = 0 then
            Void
        else
            failwith("sync should 0 element: get_map_call_ret")
      | _ ->
        failwith ("not support build in map function")
    end
  | _ -> 
    failwith ("not array error")
*)

(* run(frame_dcls): Semantic checking of a program. 
  Returns void if successful, throws an exception if something is wrong.
  Check each frame_declaration *)

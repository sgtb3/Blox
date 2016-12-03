open Ast
open Sast


(* NOT SURE IF THESE GO HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
exception Face_Taken of string;;
exception Block_Overlap of string;;


(* Facecheck algorithm marks faces that are unavailable *)
let faceCheck a = 

  for i = 0 to Array.length a - 1 do

    for j = 0 to Array.length a.(i) - 1 do

      for k = 0 to Array.length a.(i).(j) - 1 do
        let b = a.(i).(j).(k) in

        if Array.length b.faces = 6 then(

          let xblck = (if i < Array.length a - 1 then a.(i + 1).(j).(k) else {faces = [||]}) in
          let yblck = (if j < Array.length a.(i) - 1 then a.(i).(j + 1).(k) else {faces = [||]}) in
          let zblck = (if k < Array.length a.(i).(j) - 1 then a.(i).(j).(k + 1) else {faces = [||]}) in

          if Array.length xblck.faces = 6 then(
            Array.set b.faces 0 false;
            Array.set xblck.faces 1 false)
          else ignore();

          if Array.length yblck.faces = 6 then(
            Array.set b.faces 2 false;
            Array.set yblck.faces 3 false)
          else ignore();

          if Array.length zblck.faces = 6 then(
            Array.set b.faces 4 false;
            Array.set zblck.faces 5 false)
          else ignore())

        else ignore();

      done;
    done;
  done;
;;


(* Join function returns the result of joining two frames *)
let join frameA a b c d frameB e f g h =

  let aArray = Array.init frameA.x (fun i -> Array.init frameA.y (fun j -> (Array.init frameA.z (fun k -> {faces = Array.copy frameA.blocks.(i).(j).(k).faces})))) in
  let ax = a in 
  let ay = b in
  let az = c in
  let afacetr = d in
  
  let bArray = Array.init frameB.x (fun i -> Array.init frameB.y (fun j -> (Array.init frameB.z (fun k -> {faces = Array.copy frameB.blocks.(i).(j).(k).faces})))) in
  let bx = e in
  let by = f in
  let bz = g in
  let bfacetr = h in
      
  let aface =
    (if afacetr = "E" then
      aArray.(ax).(ay).(az).faces.(0)
    else if afacetr = "W" then
      aArray.(ax).(ay).(az).faces.(1)
    else if afacetr = "N" then 
      aArray.(ax).(ay).(az).faces.(2)
    else if afacetr = "S" then 
      aArray.(ax).(ay).(az).faces.(3)
    else if afacetr = "F" then 
      aArray.(ax).(ay).(az).faces.(4)
    else if afacetr = "B" then 
      aArray.(ax).(ay).(az).faces.(5)
    else false) in

  let (bface, bx_shift, by_shift, bz_shift) =
    (if bfacetr = "E" then (bArray.(bx).(by).(bz).faces.(0), (ax - 1) - bx, ay - by, az - bz)
    else if bfacetr = "W" then(bArray.(bx).(by).(bz).faces.(1), (ax + 1) - bx, ay - by, az - bz)
    else if bfacetr = "N" then(bArray.(bx).(by).(bz).faces.(2), ax - bx, (ay - 1) - by, az - bz)
    else if bfacetr = "S" then(bArray.(bx).(by).(bz).faces.(3), ax - bx, (ay + 1) - by, az - bz) 
    else if bfacetr = "F" then(bArray.(bx).(by).(bz).faces.(4), ax - bx, ay - by, (az - 1) - bz)
    else if bfacetr = "B" then(bArray.(bx).(by).(bz).faces.(5), ax - bx, ay - by, (az + 1) - bz)
    else (false, 0, 0, 0)) in
  
  (* check if frameA's block face is available *)
  if not(aface) then
    raise (Face_Taken "Specified face of block in frame A is already taken")
  else ignore();
  
  (* check if frameB's block face is available *)
  if not(bface) then 
    raise (Face_Taken "Specified face of block in frame B is already taken")
  else ignore();
  
  (* check for opposite faces *)
  if (((afacetr = "E") && not(bfacetr = "W")) ||
      ((afacetr = "W") && not(bfacetr = "E"))) then 
    prerr_string "Error: Illegal face option."
  else ignore();

  if (((afacetr = "N") && not(bfacetr = "S")) ||
      ((afacetr = "S") && not(bfacetr = "N"))) then 
    prerr_string "Error: Illegal face option."
  else ignore();
  
  if (((afacetr = "F") && not(bfacetr = "B")) ||
      ((afacetr = "B") && not(bfacetr = "F"))) then 
    prerr_string "Error: Illegal face option."
  else ignore();

  (*  ========== ALL CHECKS PASSED. BEGIN JOIN PROCESS ========== *)

  (* Determine shift values for A and B *)
  let (ax_shift, bx_shift) =
    (if bx_shift < 0 then (-bx_shift, 0) else (0, bx_shift)) in

  let (ay_shift, by_shift) =
    (if by_shift < 0 then (-by_shift, 0) else (0, by_shift)) in

  let (az_shift, bz_shift) =
    (if bz_shift < 0 then (-bz_shift, 0) else (0, bz_shift)) in

  
  (* Determine size of new array *)
  let cx_max = (max (frameA.x + ax_shift) (frameB.x + bx_shift)) in
  let cy_max = (max (frameA.y + ay_shift) (frameB.y + by_shift)) in
  let cz_max = (max (frameA.z + az_shift) (frameB.z + bz_shift)) in


  (* Create new array of blocks *)
  let c = Array.init cx_max (fun _ -> Array.init cy_max (fun _ -> (Array.init cz_max 
    (fun _ -> let b = {faces = [||] } in b )))) in

  (* Fill c with blocks from array A *)
  for i = 0 to frameA.x - 1 do

    for j = 0 to frameA.y- 1 do

      for k = 0 to frameA.z - 1 do

        let b = (Array.get (Array.get (Array.get aArray i) j) k) in
        if Array.length b.faces = 6 then
          (Array.set (Array.get (Array.get c (i + ax_shift)) (j + ay_shift)) (k + az_shift) b)
        else ignore();

      done;
    done;
  done;


  (* Fill c with blocks from array B *)
  for i = 0 to frameB.x - 1 do

    for j = 0 to frameB.y - 1 do

      for k = 0 to frameB.z - 1 do

        let bb = (Array.get (Array.get (Array.get bArray i) j) k) in
        if Array.length bb.faces = 6 then(
          if Array.length (Array.get (Array.get (Array.get c (i + bx_shift)) (j + by_shift)) (k + bz_shift)).faces = 0 then
            (Array.set (Array.get (Array.get c (i + ax_shift)) (j + ay_shift)) (k + az_shift) bb)
          else raise(Block_Overlap "The specified join causes overlap"))
        else ignore();

      done;
    done;
  done;


  (* Run faceCheck *)
  faceCheck c;


  (* Create new frame to return *)
  let frameC = {
  x = cx_max;
  y = cy_max;
  z = cz_max; 
  blocks = c;
  } in

  frameC
;;



module StringMap = Map.Make(String)

let run (stmts) =                  (* let check (globals, functions) = (* *) *)

  let report_duplicate exceptf list = (* Raise an exception if the given list has a duplicate frames *)
    let rec helper = function
	     n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (*let check_assign lvaluet rvaluet err =  (* Raise an excp if given rvalue type can't be assigned to the given lvalue type *)
    if lvaluet == rvaluet then 
      lvaluet 
    else 
      raise err
  in*)

  let rec fr_type = function 
    Fr_decl(_,_,_,name) -> name
  in

  report_duplicate (fun n -> "duplicate frame " ^ n) (List.map fr_type stmts);


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

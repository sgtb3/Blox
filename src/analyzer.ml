(* Blox Sematically checked AST - taken from Fly Language *)

open Ast

type expr =
    Literal of  int
  | BoolLit of  bool
  | Float   of  float
  | Null    of  typ
  | Id      of  string * typ (* id token *)
  | Objid   of (string * string) * typ
  | Set     of  expr list * typ
  | Map     of (expr * expr) list * typ
  | Array   of  expr list * typ
  | Binop   of (expr * op * expr) * typ
  | Unop    of (uop * expr) * typ
  | Assign  of (string * expr) * typ
  | Call    of (string * expr list) * typ
  | ObjGen  of typ * typ

let get_expr_type_info epr = match epr with
    Literal _     -> Int
  | BoolLit _     -> Bool
  | Float   _     -> Float
  | Null    x     -> x
  | Id     (_, x) -> x
  | Objid  (_, x) -> x
  | Set    (_, x) -> x
  | Map    (_, x) -> x
  | Array  (_, x) -> x
  | Binop  (_, x) -> x
  | Unop   (_, x) -> x
  | Assign (_, x) -> x
  | Call   (_, x) -> x
  | ObjGen (_, x) -> x

type stmt =
    Block  of stmt list
  | Expr   of expr
  | If     of expr * stmt list   * stmt list
  | For    of expr * expr * expr * stmt list
  | While  of expr * stmt list
  | Return of expr
  | Break
  | Continue

(* this is for lambda decl, with type information*)
(*
  type lambda_decl = 
    {
      lkey     : string; (* for matching *)
      ltyp     : typ;
      lfname   : string; (* random hash *)
      lformals : (typ * string) list;
      lbody    : stmt list;
      lret     : typ     (* the return value *)
    }
*)
type func_decl = 
  {
    key     : string; (* for matching *)
    typ     : typ;
    fname   : string;
    formals : (typ * string) list;
    body    : stmt list;
    ret     : typ     (* the return value type *)
  }

(* just raw fdecl *)
let new_null_fdecl() =
  {
    key     = "";
    typ     = Null;
    fname   = "";
    formals = [];
    body    = [];
    ret     = Null;
  }

(*raw fdecl with type*)
let new_raw_type_fdecl thistype =
  {
    key     = "";
    typ     = thistype;
    fname   = "";
    formals = [];
    body    = [];
    ret     = thistype;
  }

let compare_and_update fdecl thistype =
    match fdecl with
    | {key=a;typ=b;fname=c;formals=d;body=e;ret=rtype;}->
        begin match rtype with
        | Null -> (* We don't have Undef in our lang *)
            {key=a;typ=thistype;fname=c;formals=d;body=e;ret=thistype}
        | x -> if x = thistype then fdecl
                else failwith ("return with different type")
        end

let get_func_result fdecl = match fdecl with
    | {ret=rtype;_} -> rtype

let check_bool this_type =
    if this_type = Bool 
      then ()
    else 
      failwith ("check bool error")

(* from a stmts list get a return stmt and get the return type *)
let rec get_rtype stmt_list = match stmt_list with
    | []            -> Void               (* no return stmts just return void *)
    | (Return x::y) -> get_expr_type_info x
    | (x :: y)      -> get_rtype y

(* debug code for sast *)




(* Semantic Analyzer for the Blox compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
	     n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
  
  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
  in
   
  (**** Checking Global Variables ****)

  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let built_in_decls =  StringMap.add "print"
     { typ = Void; fname = "print"; formals = [(Int, "x")];
       locals = []; body = [] } (StringMap.singleton "printb"
     { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] })
   in
     
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
	StringMap.empty (globals @ func.formals @ func.locals )
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
				Literal _ -> Int
      | BoolLit _ -> Bool
			| Float   _ -> Float
      | Id s 			-> type_of_identifier s
			| Objid s*s	-> type_of_identifier s
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
	(match op with
          Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
	| Equal | Neq when t1 = t2 -> Bool
	| Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
	| And | Or when t1 = Bool && t2 = Bool -> Bool
        | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Unop(op, e) as ex -> let t = expr e in
	 (match op with
	   Neg when t = Int -> Int
	 | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				     " = " ^ string_of_typ rt ^ " in " ^ 
				     string_of_expr ex))
      | Call(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun (ft, _) e -> let et = expr e in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
           fd.typ
    in

    let check_bool_expr e = if expr e != Bool
     then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
     else () in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
	Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.typ then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
           
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
    in

    stmt (Block func.body)
   
  in
  List.iter check_function functions

(*get the return type of array, fail if not ok*)
let get_arr_call_ret (thistype:typ) fname expr_types = match thistype with
    | Array x ->
        let expr_len = List.length expr_types
        in
        begin match fname with
        | "push_back" ->
            if expr_len = 1 then
                if [x] = expr_types then Void
                else failwith ("type not consistent: get_arr_call_ret")
            else
                failwith ("push_back not 1 element: get_arr_call_ret")
        | "push_vec" ->
            if expr_len = 1 then
                let y = List.hd expr_types
                in
                match y with
                | Array z -> if x = z then Void
                    else failwith ("type not consistent: get_arr_call_ret")
                | _ -> failwith ("type not consistent: get_arr_call_ret")
            else
                failwith ("push_vec not 1 element: get_arr_call_ret")
        | "get_at" ->
            if expr_len = 1 then
                if [Int] = expr_types then x
                else failwith ("type not consistent: get_arr_call_ret")
            else
                failwith ("get_at not 1 element: get_arr_call_ret")
        | "set_at" ->
            if expr_len = 2 then
                if [Int;x] = expr_types then Void
                else failwith ("type not consistent: get_arr_call_ret")
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
    | _ -> failwith ("not array error")

(*get the return type of map, fail if not ok*)
let get_map_call_ret (thistype:typ) fname expr_types = match thistype with
    | Map (x,y) ->
        let expr_len = List.length expr_types
        in
        begin match fname with
        | "insert" ->
            if expr_len = 2 then
                if [x;y] = expr_types then Void
                else failwith ("type not consistent: get_map_call_ret")
            else
                failwith ("insert not 2 element: get_map_call_ret")
        | "get" ->
            if expr_len = 1 then
                if [x] = expr_types then y
                else failwith ("type not consistent: get_map_call_ret")
            else
                failwith ("get_at not 1 element: get_map_call_ret")
        | "size" ->
            if expr_len = 0 then
                Int
            else
                failwith("size should 0 element: get_map_call_ret")
        | "delete" ->
            if expr_len = 1 then
                if [x] = expr_types then Void
                else failwith ("type not consistent: get_map_call_ret")
            else
                failwith("delete should 1 element: get_map_call_ret")
        | "exist" ->
            if expr_len = 1 then
                if [x] = expr_types then Bool
                else failwith  ("type not consistent: get_map_call_ret")
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
    | _ -> failwith ("not array error")

(* Blox Sematically checked AST - taken from Fly Language *)

open Ast

type expr =
  Literal          of int
  | BoolLit        of bool
  | Float          of float
	| Null 					 of typ
  | Id             of string * typ(* id token *)
	| Objid   			 of string * string * typ
  | Set            of expr list * typ
  | Map	           of (expr * expr) list * typ
  | Array  	       of expr list * typ
  | Binop          of (expr * op * expr) * typ
  | Unop        	 of (uop * expr) * typ
	| Assign         of (string * expr) * typ
  | Call           of (string * expr list) * typ

let get_expr_type_info epr = match epr with
	| Literal _             -> Int
	| BoolLit _             -> Bool
	| Float _               -> Float
	| Null x								-> x
	| Id (_, x)             -> x
	| Objid (_, x)          -> x
	| Set (_, x)            -> x
	| Map (_, x)            -> x
	| Array (_, x)          -> x
	| Binop (_, x)          -> x
	| Unop (_, x)           -> x
	| Assign (_, x)         -> x
	| Call (_, x)           -> x

type stmt =
  Block     of  stmt list
  | Expr    of  expr
	| If      of  expr * stmt list * stmt list
  | Return  of  expr
  | For     of  expr * expr * expr * stmt list
  | While   of  expr * stmt list
  | Break
  | Continue

(* this is for lambda decl, with type information*)
type lambda_decl = {
        lkey     : string; (*for matching*)
				ltyp			: typ;
        lfname   : string; (* random hash *)
        lformals : (typ * string) list;
        lbody    : stmt list;
        lret     : typ (* the return value*)
    }

type func_decl = {
        key     : string; (* for matching*)
				typ			: typ;
        fname   : string;
        formals : (typ * string) list;
        body    : stmt list;
        ret     : typ (*the return value type*)
    }

(* just raw fdecl *)
let new_null_fdecl() =
    {
        key    = "";
        fname   = "";
        formals = [];
        body    = [];
        ret     = Undef;
    }

(*raw tfdecl with type*)
let new_raw_type_fdecl thistype =
    {
        key    = "";
        fname   = "";
        formals = [];
        body    = [];
        ret     = thistype;
    }

let compare_and_update fdecl thistype =
    match fdecl with
    | {key=a;fname=b;formals=c;body=d;ret=rtype;}->
        begin match rtype with
        | Undef ->
            {key=a;fname=b;formals=c;body=d;ret=thistype}
        | x -> if x = thistype then fdecl
                else failwith ("return with different type")
        end

let get_func_result fdecl = match fdecl with
    | {ret=rtype;_} -> rtype

let check_bool this_type =
    if this_type = Bool then ()
    else failwith ("check bool error")

(*from a stmts list get a return stmt and get the return type*)
let rec get_rtype stmt_list = match stmt_list with
    | [] -> Void (*no return stmts just return void*)
    | (Return x::y) -> get_expr_type_info x
    | (x :: y) -> get_rtype y

(* debug code for sast*)
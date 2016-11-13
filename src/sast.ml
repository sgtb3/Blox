(* Blox Sematically checked AST - taken from Fly Language *)

open Ast

type expr =
  Literal          of int
  | BoolLit        of bool
  | Float          of float
  | Id             of string * typ(* id token *)
	| Objid   			 of string * string * typ
  | Set            of expr list * typ
  | Map	           of (expr * expr) list * typ
  | Array  	       of expr list * typ
  | Binop          of (expr * op * expr) * typ
  | Unop        	 of (uop * expr) * typ
	| Assign         of (string * expr) * typ
  | Call           of (string * expr list) * typ

let get_expr_type_info tepr = match tepr with
    | TLiteral _            -> Int
    | TBoolLit _            -> Bool
    | TFloat _              -> Float
    | TNull x               -> x (*nullpointer now*)
    | TString _             -> String
    | TId (_, x)            -> x
    | TSet (_, x)           -> x
    | TMap (_, x)           -> x
    | TArray (_, x)         -> x
    | TBinop (_, x)         -> x
    | TUnop (_, x)          -> x
    | TCall (_, x)          -> x
    | TObjCall (_, x)       -> x
    | TFunc (_, x)          -> x
    | TAssign (_, x)        -> x
    | TListComprehen (_, x) -> x
    | TExec (_, x)          -> x
    | TDispatch (_, x)      -> x
    | TRegister (_, x)      -> x
    | TChangen (_, x)       -> x
    | TChanunop (_, x)      -> x
    | TChanbinop (_, x)     -> x
    | TFly (_, x)           -> x
    | TFlyo (_, x)          -> x
    | TObjGen (_, x)        -> x
    | TObjid (_, x)         -> x
    | TMAssign (_, x)       -> x

type tstmt =
  TBlock     of tstmt list
  | TExpr    of texpr
  | TReturn  of texpr
  | TIf      of texpr * tstmt list * tstmt list
  | TFor     of texpr * texpr * texpr * tstmt list
  | TForeach of string * texpr * tstmt list (*for each*)
  | TWhile   of texpr * tstmt list
  | TBreak
  | TContinue



(* this is for lambda decl, with type information*)
type t_lambda_decl = {
        ltkey     : string; (*for matching*)
        ltfname   : string; (* random hash *)
        ltbinds   : (string * typ) list;
        ltformals : (string * typ) list;
        ltbody    : tstmt list;
        ltret     : typ (* the return value*)
    }

type t_func_decl = {
        ttkey    : string; (* for matching*)
        tfname   : string;
        tformals : (string * typ) list;
        tbody    : tstmt list;
        tret     : typ (*the return value type*)
    }


(* just raw t_fdecl *)
let new_null_tfdecl() =
    {
        ttkey    = "";
        tfname   = "";
        tformals = [];
        tbody    = [];
        tret     = Undef;
    }

(*raw tfdecl with type*)
let new_raw_type_tfdecl thistype =
    {
        ttkey    = "";
        tfname   = "";
        tformals = [];
        tbody    = [];
        tret     = thistype;
    }

let compare_and_update tfdecl thistype =
    match tfdecl with
    | {ttkey=a;tfname=b;tformals=c;tbody=d;tret=rtype;}->
        begin match rtype with
        | Undef ->
            {ttkey=a;tfname=b;tformals=c;tbody=d;tret=thistype}
        | x -> if x = thistype then tfdecl
                else failwith ("return with different type")
        end


let get_func_result tfdecl = match tfdecl with
    | {tret=rtype;_} -> rtype

let check_bool this_type =
    if this_type = Bool then ()
    else failwith ("check bool error")

(*from a stmts list get a return stmt and get the return type*)
let rec get_rtype stmt_list = match stmt_list with
    | [] -> Void (*no return stmts just return void*)
    | (TReturn x::y) -> get_expr_type_info x
    | (x :: y) -> get_rtype y


type t_class_decl = {
        tcname       : string;
        member_binds : (string * typ) list;
        t_func_decls : t_func_decl list;
        (* member functions with overloading records*)
    }

(* debug code for sast*)
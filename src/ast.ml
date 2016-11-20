(* ocamlyacc Abstract Syntax Tree for Blox *)

type op = Add | Equal | Less 
(*| Sub 
  | Mult 
  | Div 
  | Neq 
  | Leq 
  | Greater 
  | Geq 
  | FrameEq
  | And 
  | Or *)

type uop = Neg  (* | Not  *)
type typ = Int | Frame | Array of typ | Default of string | Void

  (* | Bool
  | Float
  | Null
  | String
  | ObjGen  of typ
  | Array   of typ
  | Set     of typ
  | Map     of typ * typ *)
  
type blck = {
  faces : bool array;
}

type join = {
  f1_name : string;
  f1_loc  : int * int * int * string;
  f2_name : string;
  f2_loc  : int * int * int * string;
}

type frame = {
  x       : int;
  y       : int;
  z       : int;
  name    : string;
  blocks  : blck array array array;
  joins   : join array;
}

type bind = typ * string

type expr = 
    Literal of int
  | Id      of string
  | Array   of expr list
  | Unop    of uop    * expr
  | Assign  of string * expr
  | Call    of string * expr list
  | Noexpr

  (* | BoolLit of bool *)
  (* | Float   of float *)
  (* | Null    of string nullpointer belong to Default s *)
  (* | Objid   of string * string *)
  (* | ObjGen  of typ *)
  (* | Set     of expr list *)
  (* | Map     of (expr * expr) list *)
  
  (* | String  of string (*represent const string*) *)
  (* | Binop   of expr   * op * expr *)
  
type stmt = 
    Expr of expr
  | Return of expr
  (* | Block  of stmt list *)
  (* | If     of expr * stmt * stmt *)
  (* | For    of expr * expr * expr * stmt *)
  (* | While  of expr * stmt *)
  
  (* | Break *)
  (* | Continue *)
(* 
type func_decl = { 
  typ     : typ;
  fname   : string;
  formals : bind list;
  locals  : bind list;
  body    : stmt list;
} *)

type program = bind list

(* type program = bind list * func_decl list *)

(* Pretty-printing functions *)
(* let string_of_op = function
  | Add   -> "+"
  | Less  -> "<"
  | Equal -> "==" *)

(* | Sub     -> "-"
  | Mult    -> "*"
  | Div     -> "/"
  | Neq     -> "!="
  | FrameEq -> ".="
  | Leq     -> "<="
  | Greater -> ">"
  | Geq     -> ">="
  | And     -> "&&"
  | Or      -> "||" *)
(* 
let string_of_uop = function
    Neg -> "-"
  (* | Not -> "!" *)
 *)
(* 
let rec string_of_typ = function
    Int       -> "int"
  | Frame     -> "Frame"
  | Void      -> "void"
  | Array x   -> "Array_" ^ (string_of_typ x)
	| Default x -> x


  | _ -> raise (Failure ("ast.ml: string_of_typ: unsupported type"))
  | Bool       -> "bool"
  | Float      -> "float"
  | Null       -> "Null"
  | String     -> "String"
  | Set x      -> "Set_"   ^ (string_of_typ x)
  | Map (x, y) -> "Map_"   ^ (string_of_typ x) ^ "_" ^ (string_of_typ y)
   *)

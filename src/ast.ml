(* ocamlyacc Abstract Syntax Tree for Blox *)

type op = 
  Add 
| Sub 
| Mult 
| Div 
| Equal 
| Neq 
| Less 
| Leq 
| Greater 
| Geq 
| FrameEq
| And 
| Or
| Mod

type uop = 
  Neg 
| Not 

type typ = 
  Int
| Bool
| Void
| Float
| String
| Frame
| Array of typ
| Set   of typ
| Map   of typ * typ

(*
type block = {
  open_faces  : bool  array;
  face_colors : float array;
}
*)
(*type frame = {
  x       : int;
  y       : int;
  z       : int;
  name    : string;
  (*blocks  : block array array array;*)
  (*joins : DynArray*)
}
*)

type bind = typ * string

type expr = 
  Literal of int
| BoolLit of bool
| Float   of float
| Null 		of typ 
| Id      of string
| Objid   of string * string
| Set     of expr list
| Map     of (expr * expr) list
| Array   of expr list
| Binop   of expr * op * expr
| Unop    of uop * expr
| Assign  of string * expr
| Call    of string * expr list
| Noexpr

type stmt = 
  Block  of stmt list
| Expr   of expr
| If     of expr * stmt * stmt
| For    of expr * expr * expr * stmt
| While  of expr * stmt
| Return of expr
| Break
| Continue

type func_decl = { 
  typ     : typ;
  fname   : string;
  formals : bind list;
  locals  : bind list;
  body    : stmt list;
}

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add     -> "+"
  | Sub     -> "-"
  | Mult    -> "*"
  | Div     -> "/"
  | Equal   -> "=="
  | Neq     -> "!="
  | FrameEq -> ".="
  | Less    -> "<"
  | Leq     -> "<="
  | Greater -> ">"
  | Geq     -> ">="
  | And     -> "&&"
  | Or      -> "||"
  | Mod     -> "%"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_typ = function
    Int        -> "int"
  | Bool       -> "bool"
  | Void       -> "void"
  | Float      -> "float"
  | String     -> "String"
  | Frame      -> "Frame"
  | Array x    -> "Array_" ^ (string_of_typ x)
  | Set x      -> "Set_"   ^ (string_of_typ x)
  | Map (x, y) -> "Map_"   ^ (string_of_typ x) ^ "_" ^ (string_of_typ y)
  (*| _          -> raise (Failure ("ast.ml: string_of_typ: unsupported type"))*)

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

type uop = 
  Neg 
| Not 

type typ = 
  Int 
| Bool 
| Void
| Float
| Block
| Frame
| String
| Array of typ 
| Set of typ

type block = {
        open_faces : bool array;
        face_colors : float array;
}

type frame = {
        x : int;
        y : int;
        z : int;
        name : string;
        blocks : block array array array;
        (*joins : DynArray*)
}

type bind = typ * string

type expr = 
  Literal of int          
| BoolLit of bool
| Float of float
| Id of string              
| Noexpr 
| Binop of expr * op * expr 
| Unop of uop * expr 
| Assign of string * expr   
| Call of string * expr list

type stmt = 
  Block of stmt list        
| Expr of expr 
| If of expr * stmt * stmt 
| For of expr * expr * expr * stmt 
| While of expr * stmt      
| Return of expr

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
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | FrameEq -> ".="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | Float -> "float"
  | Block -> "Block"
  | Frame -> "Frame"
  | String -> "String"
  | Array x -> "array_" ^ (string_of_typ x)
  | Set x -> "set_" ^ (string_of_typ x)
  | _ -> raise (Failure ("ast.ml: string_of_typ not yet support this type"))



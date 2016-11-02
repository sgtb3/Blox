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
| BlockEq
| And 
| Or 

type uop = 
  Neg 
| Not 

type typ = 
  Int 
| Bool 
| Void 
| Frame

type bind = typ * string

type expr = 
  Literal of int            
| BoolLit of bool 
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
  | BlockEq -> ".="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | Frame -> "Frame"
  

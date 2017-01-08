(* binary operators *)
type op = 
  | And   | Or  | Mod
  | Add   | Sub | Mult | Div 
  | Equal | Neq | Less | Leq | Greater | Geq | FrameEq

(* unary operators *)
type uop = Neg | Not 

(* block face identifier - if form is Face<x,y,z,d> A; then id = ""; 
                           if form is Face A = B;      then id = A *)
type face_id = {
  dim   : int * int * int;
  face  : string;
  fc_id : string
}

(* actual block *)
type blck = {
  faces : bool array;
}

(* actual frame - if form is Frame<x,y,z> A; then id = ""; 
                  if form is Frame A = B;    then id = A  *)
type frame = {
  x      : int;
  y      : int;
  z      : int;
  blocks : blck array; 
  fr_id  : string 
}

(* all types *)
type dtype = 
  | Int | Bool | Float | String | Void
  | Frame  of frame
  | FaceId of face_id
  | Array  of dtype * int * string

(* built-in function call parameters *)
type join  = frame * face_id      * frame * face_id
type build = frame * face_id list * frame * face_id list

(* variable declarations *)
type var_decl = dtype * string

(* expressions *)
type expr =
  | Id         of string
  | Lit_Int    of int
  | Lit_Flt    of float
  | Lit_Str    of string
  | Lit_Bool   of bool
  | Assign     of string * expr
  | Fr_Assign  of string * expr
  | Fc_Assign  of string * expr
  | Binop      of expr   * op * expr
  | Unop       of uop    * expr
  | Call       of string * expr list
  | Null
  | Noexpr

(* variable assignments *)
type var_assign = dtype * string * expr

(* statements *)
type stmt =
  | Block      of stmt list
  | Expr       of expr
  | Join       of join
  | Build      of build
  | Print      of expr
  | Array      of dtype * int * string
  | Convert    of frame
  | Var_Decl   of var_decl
  | Var_Assign of var_decl * expr
  | Return     of expr
  | If         of expr * stmt * stmt
  | For        of expr * expr * expr * stmt
  | While      of expr * stmt
  | Break
  | Continue

(* frame and face assignments *)
type fr_assign = string * string
type fc_assign = string * string

(* function declarations *)
type func_decl = { 
  typ     : dtype;
  fname   : string;
  formals : var_decl list;
  body    : stmt     list;
}

(* gloabls *)
type globals = 
  | VarDecl   of var_decl 
  | VarAssign of var_assign 
  | FrAssign  of fr_assign  
  | FcAssign  of fc_assign 

(* blox program is a tuple of a globals list and a function decls list *)
type program = globals list * func_decl list

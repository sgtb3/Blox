(* binary operators *)
type binop =
  | And   | Or  | Mod
  | Add   | Sub | Mult | Div
  | Equal | Neq | Less | Leq | Greater | Geq 

(* unary operators *)
type uniop = Neg | Not

(* block face identifier *)
type face_id = {
  fc_dim : int * int * int;
  face   : string;
}

(* block *)
type blck = {
  faces : bool array;
}

(* frame *)
type frame = {
  fr_dim : int * int * int;
  blocks : blck array;
}

(* all types *)
type dtype =
  | Int | Bool | Float | String | Void
  | Frame  of frame
  | FaceId of face_id
  | Array  of dtype * int 

(* variable declarations *)
type var_decl = dtype * string

(* expressions *)
type expr =
  | Id       of string
  | Lit_Int  of int
  | Lit_Flt  of float
  | Lit_Str  of string
  | Lit_Bool of bool
  | Assign   of string * expr
  | Binop    of expr   * binop * expr
  | Unop     of uniop  * expr
  | Call     of string * expr list
  | Null
  | Noexpr

(* variable assignments *)
type var_assign = dtype * string * expr

(* statements *)
type stmt =
  | Block   of stmt list
  | Expr    of expr
  | VDecl   of var_decl
  | VAssign of var_decl * expr
  | Return  of expr
  | If      of expr * stmt * stmt
  | For     of expr * expr * expr * stmt
  | While   of expr * stmt
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
  | VarDecl   of var_decl   (* int/bool/string/float/Frame/Face/[] a; *)
  | VarAssign of var_assign (* int b = a; *)
  | FrAssign  of fr_assign  (* Frame<1,2,3> fr; *)
  | FcAssign  of fc_assign  (* Face<1,2,3,E> fc; *)

(* blox program is a tuple of a globals list and a function decls list *)
type program = globals list * func_decl list

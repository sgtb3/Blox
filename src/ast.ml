(* binary operators *)
type op = 
    And   | Or  | Mod
  | Add   | Sub | Mult | Div 
  | Equal | Neq | Less | Leq | Greater | Geq | FrameEq

(* unary operators *)
type uop = Neg | Not 

(* block face identifier *)
type face_id = {
  dim  : int * int * int;
  face : string;
}

(* join arguments *)
type join_arg = {
  frname    : string;
  blck_face : face_id list;
}

(* join function *)
type join = {
  fr_a : join_arg;
  fr_b : join_arg;
}

(* type constructors -  basic/primitive types hold literal values *)
type typ = 
    Int | Bool | Float | String | Void
  | Array of typ * string                      
  | Set of typ * string

(* actual block *)
type blck = {
  faces : bool array;
}

(* actual frame *)
type frame = {
  x : int;
  y : int;
  z : int;
  blocks : blck;
}

(* frame declaration *)
type fr_decl = {
  x : int;
  y : int;
  z : int;
  fr_name : string;
}

(* expressions *)
type expr =
    Id of string
  | Lit_Int of int
  | Lit_Bool of bool
  | Assign of string * expr
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Call of string * expr list
  | Null
  | Noexpr

(* statements *)
type stmt =
    Block of stmt list
  | Expr of expr
  | Join of join_arg * join_arg
  | Fr_decl of fr_decl
  | Fr_print of string
  | Break
  | Continue

(* was "bind" *)
type var_decl = typ * string

(* variable assignment  *)
type var_assign = typ * string * expr

(* function declaration *)
type func_decl = { 
  typ          : typ;
  fname        : string;
  formals      : var_decl list;
  loc_var_decl : var_decl list;
  loc_var_assn : var_assign list;
  body         : stmt list;
}



(* frame assignment - might need to be frame * frame *)
type fr_assign = string * string

(* gloabls is a combination of var & frame declarations and assignments *)
type globals = {
  var_decls  : var_decl list;
  var_assgns : var_assign list;
  fr_decls   : fr_decl list;
  fr_assgns  : fr_assign list;
}

(* a blox program is a tuple of globals and function declarations *)
type program = globals list * func_decl list

(* print operators *)
let string_of_op = function
    Add     -> "+"
  | Sub     -> "-"
  | Mult    -> "*"
  | Div     -> "/"
  | Equal   -> "=="
  | Neq     -> "!="
  | Less    -> "<"
  | Leq     -> "<="
  | Greater -> ">"
  | Geq     -> ">="
  | And     -> "&&"
  | Or      -> "||"
  | FrameEq -> ".="
  | Mod     -> "%"

(* print unary operators *)
let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

(* print expressions *)
let rec string_of_expr = function         
    Lit_Int(x)      -> string_of_int x
  | Id(x)           -> x
  | Lit_Bool(true)  -> "true"
  | Lit_Bool(false) -> "false"
  | Assign(x,y)     -> "Frame " ^ x ^ " = " ^ string_of_expr y ^ ";"
  | Null            -> "null"
  | Binop(e1,o,e2)  -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o,e)       -> string_of_uop o   ^ string_of_expr e
  | Call(f,el)      -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr          -> ""

(* print frame declarations *)
let string_of_frdecl frd =         
  "Frame<" ^ string_of_int frd.x ^ "," ^
             string_of_int frd.y ^ "," ^
             string_of_int frd.z ^ "> " ^
             frd.fr_name ^ ";\n"

(* print dimensions *)
let string_of_dim (x,y,z) =
  string_of_int x ^ "," ^
  string_of_int y ^ "," ^
  string_of_int z

(* print block face identifier *)
let string_of_face_id f =
  "(" ^  string_of_dim f.dim ^ f.face ^ ")"

(* print list of face_id's *)
let string_of_face_id_list fid =
  String.concat "," (List.rev (List.map string_of_face_id fid))

(* print list of join args *)
let string_of_join_arg x =
  x.frname ^ ", {" ^ string_of_face_id_list x.blck_face ^ "}"

(* print statements *)
let rec string_of_stmt = function 
    Fr_decl(fr)     -> string_of_frdecl fr
  | Block(stmts)    -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr)      -> string_of_expr expr ^ "\n"
  | Join(x,y)       -> "Join(" ^ string_of_join_arg x ^ ", " ^ string_of_join_arg y ^ ")\n"
  | Fr_print(fname) -> "print " ^ fname ^ ";\n"
  | Break           -> "break;\n"
  | Continue        -> "continue;\n"

(* print types *)
let rec string_of_typ = function
    Int           -> "int"
  | Bool          -> "bool"
  | String        -> "string"
  | Void          -> "void"
  | Float         -> "float"
  | Array(t,id)   -> (string_of_typ t) ^ "[] " ^ id
  | Set(t,id)     -> "Set<" ^ (string_of_typ t)  ^ "> " ^ id

(* print variable declarations *)
let string_of_var_decl (t,id) = string_of_typ t ^ " " ^ id ^ ";\n"

(* print variable assignment type var_assign = string * expr *) 
let string_of_vassign (t,id,exp) = string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr exp ^ ";\n"

(* print function declarations *)
let string_of_func_decl fd =
  string_of_typ fd.typ ^ " " ^ fd.fname ^ "(" ^ 
  String.concat ", " (List.map snd fd.formals)               ^ ")\n{\n" ^
  String.concat ""   (List.map string_of_var_decl fd.loc_var_decl) ^
  String.concat ""   (List.map string_of_vassign fd.loc_var_assn) ^
  String.concat ""   (List.map string_of_stmt fd.body)       ^ "}\n"

(* print frame assignments - there probably needs to be a new_frame_assign, and regular fr_assign *)
let string_of_frassign (frname1,frname2) = "Frame " ^ frname1 ^ " = " ^ frname2 ^ ";"

(* print globals *)
let string_of_globals glob = 
  String.concat "" (List.map string_of_var_decl glob.var_decls)   ^
  (* String.concat "" (List.map string_of_vassign glob.data_typs)   ^ *)
  String.concat "" (List.map string_of_vassign glob.var_assgns) ^
  String.concat "" (List.map string_of_frdecl glob.fr_decls)      ^
  String.concat "" (List.map string_of_frassign glob.fr_assgns)   ^"\n"

(* print blox program *)
let string_of_program (globals,funcs) =
  String.concat "" (List.rev (List.map string_of_globals globals)) ^ "\n" ^
  String.concat "" (List.rev (List.map string_of_func_decl funcs))

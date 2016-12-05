(* binary operators *)
type op = 
  | And   | Or  | Mod
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

(* type constructors *)
type typ = 
  | Int | Bool | Float | String | Void (* basic/primitive types hold literal values *)
  | Array of typ                       
  | Set of typ
  | Map of typ * typ

(* actual block *)
type blck = {
  faces : bool array;
}

(* actual frame *)
type frame = {
  x : int;
  y : int;
  z : int;
  blocks : blck array array array;
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
  | Id of string
  | Lit_Int of int
  | Lit_Bool of bool
  | Assign of string * expr
  | Null

(* statements *)
type stmt =
  | Block of stmt list
  | Expr of expr
  | Join of join_arg * join_arg
  | Fr_decl of fr_decl
  | Fr_print of string
  | Break
  | Continue

(* was "bind" *)
type var_decl = typ * string

(* function declaration *)
type func_decl = { 
  typ     : typ;
  fname   : string;
  formals : var_decl list;
  locals  : var_decl list;
  body    : stmt list;
}

(* variable assignment  *)
type var_assign = string * expr

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
  | Add     -> "+"
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
  | Neg -> "-"
  | Not -> "!"

(* print expressions *)
let rec string_of_expr = function         
  | Lit_Int(x)      -> string_of_int x
  | Id(x)           -> x
  | Lit_Bool(true)  -> "true"
  | Lit_Bool(false) -> "false"
  | Assign(x, y)    -> "Frame " ^ x ^ " = " ^ string_of_expr y ^ ";"
  | Null            -> "null"

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
  | Fr_decl(fr)    -> string_of_frdecl fr
  | Block(stmts)   -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr)     -> string_of_expr expr ^ "\n"
  | Join(x,y)     -> "Join(" ^ string_of_join_arg x ^ ", " ^ string_of_join_arg y ^ ")\n"
  | Fr_print(name) -> "print " ^ name ^ ";\n"
  | Break          -> "break;\n"
  | Continue       -> "continue;\n"

(* print types *)
let rec string_of_typ = function
  | Int      -> "int"
  | Bool     -> "bool"
  | String   -> "string"
  | Void     -> "void"
  | Float    -> "float"
  | Array x  -> "Array_" ^ (string_of_typ x)
  | Set x    -> "Set<"   ^ (string_of_typ x) ^ ">"
  | Map(x,y) -> "Map<"   ^ (string_of_typ x) ^ ", " ^ (string_of_typ y) ^ ">"

(* print variable declarations *)
let string_of_var_decl (t,id) = string_of_typ t ^ " " ^ id ^ ";\n"

(* print variable assignment type var_assign = string * expr *) 
let string_of_vassign (id,exp) = id ^ " = " ^ string_of_expr exp ^ ";\n"

(* print function declarations *)
let string_of_func_decl fd =
  string_of_typ fd.typ ^ " " ^ fd.fname ^ "(" ^ 
  String.concat ", " (List.map snd               fd.formals) ^ ")\n{\n" ^
  String.concat ""   (List.map string_of_var_decl fd.locals) ^
  String.concat ""   (List.map string_of_stmt       fd.body) ^ "}\n"

(* print frame assignments - type fr_assign = string * string - there probably needs to be a new_frame_assign, and regular fr_assign *)
let string_of_frassign (frname1,frname2) = frname1 ^ " = " ^ frname2 ^ ";\n"

(* print variable declarations *)
(* let string_of_glob_var_decl (t,id) = string_of_typ t ^ " " ^ vdecls.id ^ ";\n" *)

let string_of_globals g = 
  String.concat "" (List.map snd                  g.var_decls) ^
  String.concat "" (List.map string_of_vassign   g.var_assgns) ^
  String.concat "" (List.map string_of_frdecl     g.fr_decls)   ^
  String.concat "" (List.map string_of_frassign g.fr_assgns)   ^"\n"

(*var_decls  : var_decl list;
  var_assgns : var_assign list;
  fr_decls   : fr_decl list;
  fr_assgns  : fr_assign list;*)

(* print blox program *)
let string_of_program (globals, funcs) =
  String.concat ""    (List.map string_of_globals globals) ^ "\n" ^
  String.concat "\n"  (List.map string_of_func_decl funcs)

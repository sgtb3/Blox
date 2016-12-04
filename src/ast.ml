
type op = 
  | And   | Or  | Mod
  | Add   | Sub | Mult | Div 
  | Equal | Neq | Less | Leq | Greater | Geq | FrameEq

type uop = Neg | Not 

type typ = 
  | Int | Bool | Float | String | Void
  | Array of typ
  | Set of typ
  | Map of typ * typ

(* Leave this as is *)
type blck = {
  faces : bool array;
}

(* Leave this as is *)
type frame = {
  x : int;
  y : int;
  z : int;
  blocks : blck array array array;
}

type fr_decl = {
  x : int;
  y : int;
  z : int;
  fr_name : string;
}

type face_id = {
  dim : int * int * int;
  face : string;
}

type join_arg = {
  frname : string;
  blck_face : face_id list;
}

type join = {
  fr_a : join_arg;
  fr_b : join_arg;
}

type bind = typ * string

type expr =
  | Id of string
  | Lit_Int of int
  | Lit_Bool of bool
  | Assign of string * expr
  | Null

type stmt =
  | Block of stmt list
  | Expr of expr
  | Join of join_arg * join_arg
  | Fr_decl of int * int * int * string
  | Fr_print of string
  | Break
  | Continue

type func_decl = { 
  typ : typ;
  fname : string;
  formals : bind list;
  locals : bind list;
  body : stmt list;
}

(* 'bind list' is the global variables, then the functions *)
type program = bind list * func_decl list

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
let string_of_frdecl x y z name =         
  "Frame<" ^ string_of_int x ^ "," ^
             string_of_int y ^ "," ^
             string_of_int z ^ "> " ^
             name ^ ";\n"

(* print dimensions *)
let string_of_dim (x,y,z) =
  string_of_int x ^ "," ^
  string_of_int y ^ "," ^
  string_of_int z

(* print type face_id *)
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
| Fr_decl(x, y, z, name) -> string_of_frdecl x y z name
| Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
| Expr(expr)   -> string_of_expr expr ^ "\n"
| Join(x, y)   -> "Join(" ^ string_of_join_arg x ^ ", " ^ string_of_join_arg y ^ ")\n"

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
  | Array x    -> "Array_" ^ (string_of_typ x)
  | Set x      -> "Set_"   ^ (string_of_typ x)
  | Map (x, y) -> "Map_"   ^ (string_of_typ x) ^ "_" ^ (string_of_typ y)


(* print variable declarations *)
let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

(* print function declarations *)
let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"

(* print program *)
let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

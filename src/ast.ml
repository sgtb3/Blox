(* binary operators *)
type op = 
  | And   | Or  | Mod
  | Add   | Sub | Mult | Div 
  | Equal | Neq | Less | Leq | Greater | Geq | FrameEq

(* unary operators *)
type uop = Neg | Not 

(* if form is Face<x,y,z,d> A; then id = ""; 
   if form is Face A = B;      then id = A  *)

(* block face identifier *)
type face_id = {
  dim   : int * int * int;
  face  : string;
  fc_id : string
}

(* actual block *)
type blck = {
  faces : bool array;
}

(* if form is Frame<x,y,z> A; then id = ""; 
   if form is Frame A = B;    then id = A  *)

(* actual frame *)
type frame = {
  x : int;
  y : int;
  z : int;
  blocks : blck array; 
  fr_id  : string 
}

(* All types *)
type dtype = 
  | Int | Bool | Float | String | Void
  | Frame of frame
  | FaceId of face_id
  | Array of dtype * int * string

(* built-in function call parameters *)
type join  = frame * face_id * frame * face_id
type build = frame * face_id array * frame * face_id array

(* variable declarations *)
type var_decl = dtype * string

(* expressions *)
type expr =
  | Id of string
  | Lit_Int of int
  | Lit_Flt of float
  | Lit_Str of string
  | Lit_Bool of bool
  | Assign of string * expr
  | Fr_assign of string * expr
  | Fc_assign of string * expr
  | Var_assign of dtype * string * expr
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Call of string * expr list
  | Null
  | Noexpr

(* variable assignments *)
type var_assign = dtype * string * expr

(* statements *)
type stmt =
  | Block of stmt list
  | Expr of expr
  | Join of join
  | Build of build
  | Print of expr
  | Array of dtype * int * string
  | Convert of frame
  | Var_decl of var_decl
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break
  | Continue

(* function declaration *)
type func_decl = { 
  typ     : dtype;
  fname   : string;
  formals : var_decl list;
  body    : stmt list;
}

(* frame assignment - might need to be frame * frame *)
type fr_assign = string * string

(* face assignment - might need to be frame * frame *)
type fc_assign = string * string

(* gloabls is a combination of var & frame declarations and assignments *)
type globals = {
  var_decls  : var_decl list;
  var_assgns : var_assign list;
  fr_assgns  : fr_assign list;
  fc_assgns  : fc_assign list;
}

(* a blox program is a tuple of globals and function declarations *)
type program = globals list * func_decl list

(* print binary operators *)
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
  | Neg     -> "-"
  | Not     -> "!"

let string_of_dim (x,y,z) = 
  string_of_int x ^ "," ^ 
  string_of_int y ^ "," ^ 
  string_of_int z

let rec get_type = function
  | Int          -> Int
  | Bool         -> Bool
  | String       -> String
  | Float        -> Float
  | Frame(fr)    -> Frame(fr)
  | FaceId(fc)   -> FaceId(fc)
  | Void         -> Void
  | Array(x,y,z) -> Array(x,y,z)

(* print datatypes *)
let rec string_of_dtype = function
  | Int          -> "int"
  | Bool         -> "bool"
  | String       -> "string"
  | Float        -> "float"
  | Frame(fr)    -> "Frame" ^ "<" ^ string_of_dim (fr.x, fr.y, fr.z) ^ ">"
  | FaceId(fc)   -> "Face"  ^ "<" ^ string_of_dim fc.dim ^ "," ^ fc.face ^ ">"
  | Void         -> "void"
  | Array(x,y,z) -> string_of_dtype x ^ "[" ^ string_of_int y ^ "] " ^ z

(* print expressions *)
let rec string_of_expr = function         
  | Lit_Int(x)        -> string_of_int x
  | Lit_Flt(x)        -> string_of_float x
  | Lit_Str(x)        -> x
  | Id(x)             -> x
  | Lit_Bool(true)    -> "true"
  | Lit_Bool(false)   -> "false"
  | Assign(x,y)       -> x ^ " = " ^ string_of_expr y ^ ";"
  | Fr_assign(x,y)    -> "Frame " ^ x ^ " = " ^ string_of_expr y ^ ";"
  | Fc_assign(x,y)    -> "Face " ^ x ^ " = " ^ string_of_expr y ^ ";"
  | Var_assign(x,y,z) -> string_of_dtype x ^ " " ^ y ^ " = " ^ string_of_expr z ^ ";"
  | Null              -> "null"
  | Binop(e1,o,e2)    -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o,e)         -> string_of_uop o   ^ string_of_expr e
  | Call(f,el)        -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr            -> ""

(* print variable declarations *)
let string_of_var_decl (x,y) =
  string_of_dtype x ^ " " ^ y ^ ";"

(* print block face identifiers *)
let string_of_face_id (w,x,y,z) =
  "(" ^ string_of_int w ^ ", " ^ 
        string_of_int x ^ ", " ^ 
        string_of_int y ^ ", " ^ 
        z ^ ")"

(* print Join function arguments *)
let string_of_join_args (w,x,y,z) =
  w.fr_id ^ "," ^ x.fc_id ^ "," ^ y.fr_id ^ "," ^ z.fc_id 

(* print block face identifiers - not currently used *)
let string_of_face_id_list faceid =
  "(" ^ string_of_dim faceid.dim ^ "," ^ faceid.face ^ ")"

(* print Build function arguments - will substitute the actual face values in here *)
let string_of_build_args (w,x,y,z) =
  let alist = (Array.to_list x) in
  let blist = (Array.to_list z) in
  w.fr_id ^ "," ^ (String.concat "," (List.map (fun f -> f.fc_id) alist)) ^ "," ^
  y.fr_id ^ "," ^ (String.concat "," (List.map (fun f -> f.fc_id) blist))
  
(* print statements *)
let rec string_of_stmt = function 
  | Var_decl(x,y)     -> string_of_var_decl (x,y) ^ "\n"
  | Expr(expr)        -> string_of_expr expr      ^ "\n"
  | Join(w,x,y,z)     -> "Join("  ^ string_of_join_args  (w,x,y,z) ^ ");\n"
  | Build(w,x,y,z)    -> "Build(" ^ string_of_build_args (w,x,y,z) ^ ");\n"
  | Array(x,y,z)      -> string_of_dtype x ^ "[" ^ string_of_int y ^ "] " ^ z ^";\n"
  | Print(e)          -> "print(" ^ string_of_expr e ^ ");\n"
  | Convert(fr)       -> "Convert(" ^ fr.fr_id ^ ");\n"
  | Break             -> "break;\n"
  | Continue          -> "continue;\n"
  | Return(expr)      -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e,s,Block([])) -> "if ("    ^ string_of_expr e    ^ ")\n" ^ string_of_stmt s
  | If(e,s1,s2)       -> "if ("    ^ string_of_expr e    ^ ")\n" ^ string_of_stmt s1 ^ 
                         "else\n"  ^ string_of_stmt s2
  | For(e1,e2,e3,s)   -> "for ("   ^ string_of_expr e1   ^ "; " ^ string_of_expr e2 ^ 
                         "; "      ^ string_of_expr e3   ^ ") "  ^ string_of_stmt s
  | While(e,s)        -> "while (" ^ string_of_expr e    ^ ") "  ^ string_of_stmt s
  | Block(stmts)      -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"

(* print variable assignments *) 
let string_of_vassign (t,id,exp) = 
  string_of_dtype t ^ " " ^ id ^ " = " ^ 
  string_of_expr exp ^ ";\n"

(* print frame assignments *)
let string_of_frassign (fn1,fn2) = 
  "Frame " ^ fn1 ^ " = " ^ fn2 ^ ";"

(* print face assignments *)
let string_of_fcassign (fn1,fn2) = 
  "Face " ^ fn1 ^ " = " ^ fn2 ^ ";"

(* print function declarations *)
let string_of_func_decl fd =
  string_of_dtype fd.typ ^ " " ^ fd.fname ^ "(" ^ 
  String.concat ", " (List.map snd fd.formals)         ^ ")\n{\n" ^
  String.concat ""   (List.map string_of_stmt fd.body) ^ "}\n"

(* print globals *)
let string_of_globals glob = 
  String.concat "" (List.map string_of_var_decl glob.var_decls) ^
  String.concat "" (List.map string_of_vassign glob.var_assgns) ^
  String.concat "" (List.map string_of_frassign glob.fr_assgns) ^
  String.concat "" (List.map string_of_fcassign glob.fc_assgns) ^"\n"

(* print blox program *)
let string_of_program (globals,funcs) =
  String.concat ""   (List.rev (List.map string_of_globals globals)) ^ "\n" ^
  String.concat "\n" (List.rev (List.map string_of_func_decl funcs))
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

(* frame declaration *)
type fc_decl = {
  dim  : int * int * int;
  face : string;
  fc_name : string;
}

(* frame declaration *)
type fr_decl = {
  frx : int;
  fry : int;
  frz : int;
  fr_name : string;
}

(* actual block *)
type blck = {
  faces : bool array;
}

(* actual frame *)
type frame = {
  x : int;
  y : int;
  z : int;
  blocks : blck array;
}

type typ = Int | Bool | Float | String | Void | Frame of fr_decl| FaceId of fc_decl

(* All types *)
type dtype = 
  | Typ of typ
  | Array of typ * int * string

(* built-in function calls *)
type join  = string * string * string * string
type build = string * string * string * string

(* variable declarations - was "bind" *)
type var_decl = dtype * string

(* expressions *)
type expr =
  | Id of string
  | Lit_Int of int
  | Lit_Bool of bool
  | Assign of string * expr
  | Fr_assign of string * expr
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
  | Array of typ * int * string
  | Fr_decl of fr_decl
  | Fc_decl of fc_decl
  | Convert of string
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

(* gloabls is a combination of var & frame declarations and assignments *)
type globals = {
  var_decls  : var_decl list;
  var_assgns : var_assign list;
  fr_decls   : fr_decl list;
  fr_assgns  : fr_assign list;
  fc_decls   : fc_decl list;
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
  | Neg     -> "-"
  | Not     -> "!"

(* print types *)
let rec string_of_typ = function
  | Int       -> "int"
  | Bool      -> "bool"
  | String    -> "string"
  | Void      -> "void"
  | Float     -> "float"
  | Frame(x)  -> "Frame"
  | FaceId(x) -> "Face"

let rec string_of_dtype = function
  | Typ(x)       -> string_of_typ x
  | Array(x,y,z) -> string_of_typ x ^ "[" ^ string_of_int y ^ "] " ^ z

(* print expressions *)
let rec string_of_expr = function         
  | Lit_Int(x)        -> string_of_int x
  | Id(x)             -> x
  | Lit_Bool(true)    -> "true"
  | Lit_Bool(false)   -> "false"
  | Assign(x,y)       -> x ^ " = " ^ string_of_expr y ^ ";"
  | Fr_assign(x,y)    -> "Frame " ^ x ^ " = " ^ string_of_expr y ^ ";"
  | Var_assign(x,y,z) -> string_of_dtype x ^ " " ^ y ^ " = " ^ string_of_expr z ^ ";"
  | Null              -> "null"
  | Binop(e1,o,e2)    -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o,e)         -> string_of_uop o   ^ string_of_expr e
  | Call(f,el)        -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr            -> ""

(* print frame declarations *)
let string_of_frdecl frd =         
  "Frame<" ^ string_of_int frd.frx ^ "," ^
             string_of_int frd.fry ^ "," ^
             string_of_int frd.frz ^ "> " ^
             frd.fr_name ^ ";\n"

(* print dimensions *)
let string_of_dim (x,y,z) =
  string_of_int x ^ "," ^
  string_of_int y ^ "," ^
  string_of_int z

(* print face declarations *)
let string_of_fcdecl fcd =         
  "Face<" ^ string_of_dim fcd.dim ^ "," ^
            fcd.face ^ "> " ^
            fcd.fc_name ^ ";\n"

let string_of_var_decl (x,y) =
  string_of_dtype x ^ " " ^ y ^ ";"

(* print block face identifier *)
let string_of_face_id (w,x,y,z) =
  "(" ^ string_of_int w ^ ", " ^ 
        string_of_int x ^ ", " ^ 
        string_of_int y ^ ", " ^ 
        z ^ ")"

(* print list of face_id's *)
let string_of_face_id_list fid =
  String.concat "," (List.rev (List.map string_of_face_id fid))

(* print list of join args *)
let string_of_build (w,x,y,z) =
  "Build(" ^ w ^ ", " ^ x ^ ", " ^ y ^ ", " ^ z ^ ");\n"

(* print list of join args *)
let string_of_join (w,x,y,z) =
  "Join(" ^ w ^ ", " ^ x ^ ", " ^ y ^ ", " ^ z ^ ");\n"

(* print statements *)
let rec string_of_stmt = function 
  | Fr_decl(frd)      -> string_of_frdecl frd
  | Fc_decl(fcd)      -> string_of_fcdecl fcd 
  | Var_decl(x,y)     -> string_of_var_decl (x,y)  ^ "\n"
  | Block(stmts)      -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr)        -> string_of_expr  expr      ^ "\n"
  | Join(w,x,y,z)     -> string_of_join  (w,x,y,z) ^ "\n"
  | Build(w,x,y,z)    -> string_of_build (w,x,y,z) ^ "\n"
  | Array(x,y,z)      -> string_of_typ x ^ "[" ^ string_of_int y ^ "] " ^ z ^";\n"
  | Convert(fname)   -> "Convert(" ^ fname ^ ");\n"
  | Break             -> "break;\n"
  | Continue          -> "continue;\n"
  | Return(expr)      -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e,s,Block([])) -> "if ("    ^ string_of_expr e    ^ ")\n" ^ string_of_stmt s
  | If(e,s1,s2)       -> "if ("    ^ string_of_expr e    ^ ")\n" ^ string_of_stmt s1 ^ 
                         "else\n"  ^ string_of_stmt s2
  | For(e1,e2,e3,s)   -> "for ("   ^ string_of_expr e1   ^ " ; " ^ string_of_expr e2 ^ 
                         " ; "     ^ string_of_expr e3   ^ ") "  ^ string_of_stmt s
  | While(e,s)        -> "while (" ^ string_of_expr e    ^ ") "  ^ string_of_stmt s

(* print variable assignment type var_assign = string * expr *) 
let string_of_vassign (t,id,exp) = 
  string_of_dtype t ^ " " ^ id ^ " = " ^ 
  string_of_expr exp ^ ";\n"

(* print frame assignments *)
let string_of_frassign (fn1,fn2) = 
  "Frame " ^ fn1 ^ " = " ^ fn2 ^ ";"

(* print function declarations *)
let string_of_func_decl fd =
  string_of_dtype fd.typ ^ " " ^ fd.fname              ^ "(" ^ 
  String.concat ", " (List.map snd fd.formals)         ^ ")\n{\n" ^
  String.concat ""   (List.map string_of_stmt fd.body) ^ "}\n"

(* print globals *)
let string_of_globals glob = 
  String.concat "" (List.map string_of_var_decl glob.var_decls) ^
  String.concat "" (List.map string_of_vassign glob.var_assgns) ^
  String.concat "" (List.map string_of_fcdecl glob.fc_decls)    ^
  String.concat "" (List.map string_of_frdecl glob.fr_decls)    ^
  String.concat "" (List.map string_of_frassign glob.fr_assgns) ^"\n"

(* print blox program *)
let string_of_program (globals,funcs) =
  String.concat "" (List.rev (List.map string_of_globals globals)) ^ "\n" ^
  String.concat "" (List.rev (List.map string_of_func_decl funcs))
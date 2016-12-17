(* binary operators *)
type op = 
    And   | Or  | Mod
  | Add   | Sub | Mult | Div 
  | Equal | Neq | Less | Leq | Greater | Geq | FrameEq

(* unary operators *)
type uop = Neg | Not 

(* built-in function calls *)
type join  = string * string * string * string
type build = string * string * string * string

(* actual block, frame, face id - created by analyzer *)
type blck = bool array
type frame = int * int * int * blck array
type face_id = int * int * int * string

type typ = 
    Int | Bool | Float | String | Void
  | Fr_decl of int * int * int
  | Fc_decl of int * int * int * string 
  | Array of typ * int

type var_decl = typ * string

(* expressions *)
type expr =
    Id of string
  | Lit_Int of int
  | Lit_Bool of bool
  | Lit_Float of float
  | Lit_String of string
  (* | Lit_Void of unit *)
  | Frame of frame
  | FaceId of face_id
  | Assign of string * expr
  | Fr_assign of string * expr (* check if needed *)
  | Var_assign of typ * string * expr
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Call of string * expr list
  | Null
  | Noexpr

(* statements *)
type stmt =
    Block of stmt list
  | Expr of expr
  | Join of join
  | Build of build
  | Array of typ * int * string
  (* | Fr_decl of int * int * int
  | Fc_decl of int * int * int * string  *)
  | Fr_print of string
  | Var_decl of typ * string
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break
  | Continue

(* glob frame & var assigns, glob var decls *)
type fr_assign_g  = string * string
type var_assign_g = typ * string * expr

(* function declaration *)
type func_decl = { 
  typ     : typ;
  fname   : string;
  formals : var_decl list;
  body    : stmt list;
}

(* gloabls is a combination of var & frame declarations and assignments *)
type globals = {
  var_decls  : var_decl list;
  var_assgns : var_assign_g list;
  fr_assgns  : fr_assign_g list;
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
    Neg     -> "-"
  | Not     -> "!"

(* print frame declarations *)
let string_of_frdecl (x,y,z) =         
  "Frame<" ^ string_of_int x ^ "," ^
             string_of_int y ^ "," ^
             string_of_int z ^ "> "

(* print face declarations *)
let string_of_fcdecl (w,x,y,z) =         
  "Face<" ^ string_of_int w ^ "," ^
             string_of_int x ^ "," ^
             string_of_int y ^ "," ^ z ^ "> "

(* print types *)
let rec string_of_typ = function
    Int       -> "int"
  | Bool      -> "bool"
  | String    -> "string"
  | Void      -> "void"
  | Float     -> "float"
  | Fr_decl(x,y,z) -> string_of_frdecl (x,y,z)
  | Fc_decl(w,x,y,z)-> string_of_fcdecl (w,x,y,z)
  | Array(x,y) -> string_of_typ x ^ "[" ^ string_of_int y ^ "]"

(* print expressions *)
let rec string_of_expr = function         
    Lit_Int(x)        -> string_of_int x
  | Id(x)             -> x
  | Lit_Bool(true)    -> "true"
  | Lit_Bool(false)   -> "false"
  | Lit_Float(f)      -> string_of_float f
  | Lit_String(s)     -> s
  | Frame(f)          -> "frame_lit" 
  | FaceId(face_id)   -> "face_id_lit"
  | Assign(x,y)       -> x ^ " = " ^ string_of_expr y ^ ";"
  | Fr_assign(x,y)    -> "Frame " ^ x ^ " = " ^ string_of_expr y ^ ";"
  | Var_assign(x,y,z) -> string_of_typ x ^ " " ^ y ^ " = " ^ string_of_expr z ^ ";"
  | Null              -> "null"
  | Binop(e1,o,e2)    -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o,e)         -> string_of_uop o   ^ string_of_expr e
  | Call(f,el)        -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr            -> ""

let string_of_var_decl (x,y) =
  string_of_typ x ^ " " ^ y ^ ";\n"

(* print block face identifier *)
let string_of_face_id (w,x,y,z) =
  "(" ^ string_of_int w ^ ", " ^ 
        string_of_int x ^ ", " ^ 
        string_of_int y ^ ", " ^ 
        z ^ ")"

(* print list of face_id's *)
let string_of_face_id_list fid =
  String.concat "," (List.rev (List.map string_of_face_id fid))


(* print build function *)
let string_of_build (w,x,y,z) =
  "Build(" ^ w ^ ", " ^ x ^ ", " ^
            y ^ ", " ^ z ^ ");\n"

(* print join function *)
let string_of_join (w,x,y,z) =
  "Join(" ^ w ^ ", " ^ x ^ ", " ^
            y ^ ", " ^ z ^ ");\n"

(* print statements *)
let rec string_of_stmt = function 
    Fr_decl(x,y,z)    -> string_of_frdecl (x,y,z)
  | Fc_decl(w,x,y,z)  -> string_of_fcdecl (w,x,y,z) 
  | Var_decl(x,y)     -> string_of_typ x ^ " " ^ y ^ ";\n"
  | Block(stmts)      -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr)        -> string_of_expr  expr      ^ "\n"
  | Join(w,x,y,z)     -> string_of_join  (w,x,y,z) ^ "\n"
  | Build(w,x,y,z)    -> string_of_build (w,x,y,z) ^ "\n"
  | Array(x,y,z)      -> string_of_typ x ^ "[" ^ string_of_int y ^ "] " ^ z ^";\n"
  | Fr_print(fname)   -> "print " ^ fname ^ ";\n"
  | Break             -> "break;\n"
  | Continue          -> "continue;\n"
  | Return(e)         -> "return " ^ string_of_expr e  ^ ";\n";
  | If(e,s,Block([])) -> "if ("    ^ string_of_expr e  ^ ")\n" ^ string_of_stmt s
  | If(e,s1,s2)       -> "if ("    ^ string_of_expr e  ^ ")\n" ^ string_of_stmt s1 ^ 
                         "else\n"  ^ string_of_stmt s2
  | For(e1,e2,e3,s)   -> "for ("   ^ string_of_expr e1 ^ " ; " ^ string_of_expr e2 ^ 
                         " ; "     ^ string_of_expr e3 ^ ") "  ^ string_of_stmt s
  | While(e,s)        -> "while (" ^ string_of_expr e  ^ ") "  ^ string_of_stmt s

(* print variable assignment type var_assign = string * expr *) 
let string_of_vassign (t,id,exp) = 
  string_of_typ t    ^ " " ^ id ^ " = " ^ 
  string_of_expr exp ^ ";\n"

(* print frame assignments *)
let string_of_frassign (fn1,fn2) = 
  "Frame " ^ fn1 ^ " = " ^ fn2 ^ ";"

(* print function declarations *)
let string_of_func_decl fd =
  string_of_typ fd.typ ^ " " ^ fd.fname ^ "(" ^ 
  String.concat ", " (List.map snd fd.formals) ^ ")\n{\n" ^
  String.concat ""   (List.map string_of_stmt fd.body) ^ "}\n"

(* print globals *)
let string_of_globals glob = 
  String.concat "" (List.map string_of_var_decl glob.var_decls) ^
  String.concat "" (List.map string_of_vassign glob.var_assgns) ^
  String.concat "" (List.map string_of_frassign glob.fr_assgns) ^ "\n"

(* print blox program *)
let string_of_program (globals,funcs) =
  String.concat "" (List.rev (List.map string_of_globals globals)) ^ "\n" ^
  String.concat "" (List.rev (List.map string_of_func_decl funcs))

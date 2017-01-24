open Ast

(* print binary operators *)
let string_of_op = function
  | Add     -> "+"
  | Or      -> "||"
  | Mod     -> "%"
  | And     -> "&&"
  | Sub     -> "-"
  | Mult    -> "*"
  | Div     -> "/"
  | Equal   -> "=="
  | Neq     -> "!="
  | Less    -> "<"
  | Leq     -> "<="
  | Greater -> ">"
  | Geq     -> ">="

(* print unary operators *)
let string_of_uop = function
  | Neg -> "-"
  | Not -> "!"

(* print dimensions *)
let string_of_dim (x,y,z) =
  string_of_int x ^ "," ^
  string_of_int y ^ "," ^
  string_of_int z

(* print datatypes *)
let rec string_of_dtype = function
  | Int        -> "int"
  | Bool       -> "bool"
  | String     -> "string"
  | Float      -> "float"
  | Frame(fr)  -> "Frame" ^ "<" ^ string_of_dim fr.fr_dim ^ ">"
  | FaceId(fc) -> "Face"  ^ "<" ^ string_of_dim fc.fc_dim ^ "," ^ fc.face ^ ">"
  | Void       -> "void"
  | Array(x,y) -> string_of_dtype x ^ "[" ^ string_of_int y ^ "]"

(* print expressions *)
let rec string_of_expr = function
  | Lit_Int(x)      -> string_of_int   x
  | Lit_Flt(x)      -> string_of_float x
  | Lit_Str(x)      -> x
  | Id(x)           -> x
  | Lit_Bool(true)  -> "true"
  | Lit_Bool(false) -> "false"
  | Null            -> "null"
  | Noexpr          -> ""
  | Assign(x,y)     -> x ^ " = " ^ string_of_expr y
  | Binop(e1,o,e2)  -> string_of_expr e1 ^ " " ^ string_of_op  o  ^ " " ^
                       string_of_expr e2
  | Unop(o,e)       -> string_of_uop o   ^ string_of_expr e
  | Call(f,el)      -> f ^ "(" ^ String.concat ", "
                       (List.map string_of_expr el) ^ ")"

(* print variable declarations *)
let string_of_var_decl (x,y) =
  string_of_dtype x ^ " " ^ y

let rec string_of_vd_list = function
  | []       -> ""
  | hd :: tl -> string_of_var_decl hd ^ ";" ^ string_of_vd_list tl

(* print variable assignments *)
let string_of_vassign (t,id,exp) =
  string_of_dtype t ^ " " ^ id ^ " = " ^ string_of_expr exp 

let rec string_of_va_list = function
  | []       -> ""
  | hd :: tl -> string_of_vassign hd ^ ";" ^ string_of_va_list tl

(* print statements *)
let rec string_of_stmt = function
  | Block(stmts)      -> " {\n\t" ^ String.concat ""
                         (List.map string_of_stmt stmts) ^ "\t}\n\n"
  | Expr(expr)        -> "\t" ^ string_of_expr expr      ^ ";\n"
  | VDecl(x,y)        -> "\t" ^ string_of_var_decl (x,y) ^ ";\n"
  | VAssign(va,ex)    -> "\t" ^ string_of_vassign (fst va,snd va,ex) ^ ";\n"
  | Return(expr)      -> "\treturn " ^ string_of_expr expr ^ ";\n";
  | If(e,s,Block([])) -> "\n\tif ("  ^ string_of_expr e  ^
                         ")"         ^ string_of_stmt s
  | If(e,s1,s2)       -> "\n\tif ("  ^ string_of_expr e  ^
                         ")"         ^ string_of_stmt s1 ^
                         "\telse"    ^ string_of_stmt s2
  | For(e1,e2,e3,s)   -> "\n\tfor (" ^ string_of_expr e1 ^ "; " ^ 
                          string_of_expr e2 ^ "; " ^
                          string_of_expr e3 ^ ") " ^
                          string_of_stmt s
  | While(e,s)        -> "\n\twhile (" ^ string_of_expr e  ^
                         ")"           ^ string_of_stmt s
  | Break             -> "\tbreak;\n"
  | Continue          -> "\tcontinue;\n"

(* print frame assignments *)
let string_of_frassign (fn1,fn2) =
  "Frame " ^ fn1 ^ " = " ^ fn2

let rec string_of_fra_list = function
  | []       -> ""
  | hd :: tl -> string_of_frassign hd ^ ";" ^ string_of_fra_list tl

(* print face assignments *)
let string_of_fcassign (fn1,fn2) =
  "Face " ^ fn1 ^ " = " ^ fn2 

let rec string_of_fca_list = function
  | []       -> ""
  | hd :: tl -> string_of_fcassign hd ^ ";" ^ string_of_fca_list tl

(* print function declarations *)
let string_of_func_decl fd =
  "\n" ^ string_of_dtype fd.typ ^ " " ^ fd.fname ^ "(" ^
  String.concat ", " (List.map string_of_var_decl fd.formals) ^ ") {\n" ^
  String.concat ""   (List.map string_of_stmt fd.body)        ^ "}\n"

(* print blox program *)
let string_of_program (globals,funcs) =
  String.concat "\n"
   (List.rev (List.map (fun f -> match f with
                         | VarDecl(vd)   -> string_of_vd_list  [vd]
                         | VarAssign(va) -> string_of_va_list  [va]
                         | FrAssign(fra) -> string_of_fra_list [fra]
                         | FcAssign(fca) -> string_of_fca_list [fca]
                       ) globals)) ^ "\n" ^
  String.concat "" (List.rev (List.map string_of_func_decl funcs))
